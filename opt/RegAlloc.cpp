//
//  RegAlloc.cpp
//  uscc
//
//  Implements a register allocator based on the
//  RegAllocBasic allocator from LLVM.
//---------------------------------------------------------
//  Portions of the code in this file are:
//  Copyright (c) 2003-2014 University of Illinois at
//  Urbana-Champaign.
//  All rights reserved.
//
//  Distributed under the University of Illinois Open Source
//  License.
//---------------------------------------------------------
//  Remaining code is:
//---------------------------------------------------------
//  Copyright (c) 2014, Sanjay Madhav
//  All rights reserved.
//
//  This file is distributed under the BSD license.
//  See LICENSE.TXT for details.
//---------------------------------------------------------

#include "llvm/CodeGen/Passes.h"
#include "../lib/CodeGen/AllocationOrder.h"
#include "../lib/CodeGen/LiveDebugVariables.h"
#include "../lib/CodeGen/RegAllocBase.h"
#include "../lib/CodeGen/Spiller.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/CodeGen/CalcSpillWeights.h"
#include "llvm/CodeGen/LiveIntervalAnalysis.h"
#include "llvm/CodeGen/LiveRangeEdit.h"
#include "llvm/CodeGen/LiveRegMatrix.h"
#include "llvm/CodeGen/LiveStackAnalysis.h"
#include "llvm/CodeGen/MachineBlockFrequencyInfo.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineLoopInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/RegAllocRegistry.h"
#include "llvm/CodeGen/VirtRegMap.h"
#include "llvm/PassAnalysisSupport.h"
#undef DEBUG
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetRegisterInfo.h"
#include <cstdlib>
#include <queue>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <stack>
#include <iostream>
#include <algorithm>

using namespace llvm;

#define DEBUG_TYPE "regalloc"

static FunctionPass* createUSCCRegisterAllocator();

static RegisterRegAlloc usccRegAlloc("uscc", "USCC register allocator",
									  createUSCCRegisterAllocator);

size_t NUM_COLORS = 4;
std::unordered_map<LiveInterval*, unsigned int> regStack;

namespace {
	struct CompSpillWeight {
		bool operator()(LiveInterval *A, LiveInterval *B) const {
			return regStack[A] < regStack[B];
		}
	};
}

namespace {
	struct InterferenceNode {
	    LiveInterval* interval;
	    InterferenceNode* next = nullptr;
	    InterferenceNode* prev = nullptr;
	    unsigned int neighborCount = 0;
	    std::vector<InterferenceNode*> neighbors;
	};

	class InterferenceList {
	public:
	    void removeNode(InterferenceNode*);
	    void pushBack(InterferenceNode*);
	    bool isEmpty() {return (bool)head;};
	    InterferenceNode* getHead() {return head;};
	private:
	    InterferenceNode* head = nullptr;
	    InterferenceNode* tail = nullptr;
	};
	
	/// RAUSCC allocator pass
	class RAUSCC : public MachineFunctionPass, public RegAllocBase {
		// context
		MachineFunction *MF;
		
		// Intererence Graph
		InterferenceList graph;
		InterferenceList removed;
		
		// state
		std::unique_ptr<Spiller> SpillerInstance;
		std::priority_queue<LiveInterval*, std::vector<LiveInterval*>,
			CompSpillWeight> Queue;
			
		// Scratch space.  Allocated here to avoid repeated malloc calls in
		// selectOrSplit().
		BitVector UsableRegs;
			
		public:
		RAUSCC();
			
		/// Return the pass name.
		const char* getPassName() const override {
		  return "Basic Register Allocator";
		}
			
		/// RAUSCC analysis usage.
		void getAnalysisUsage(AnalysisUsage &AU) const override;
			
		void releaseMemory() override;
			
		Spiller &spiller() override { return *SpillerInstance; }
			
		void enqueue(LiveInterval *LI) override {
		  Queue.push(LI);
		}
			
		LiveInterval *dequeue() override {
		  if (Queue.empty())
			  return nullptr;
		  LiveInterval *LI = Queue.top();
		  Queue.pop();
		  return LI;
		}
			
		unsigned selectOrSplit(LiveInterval &VirtReg,
							 SmallVectorImpl<unsigned> &SplitVRegs) override;
			
		/// Perform register allocation.
		bool runOnMachineFunction(MachineFunction &mf) override;
			
		// Helper for spilling all live virtual registers currently unified under preg
		// that interfere with the most recently queried lvr.  Return true if spilling
		// was successful, and append any new spilled/split intervals to splitLVRs.
		bool spillInterferences(LiveInterval &VirtReg, unsigned PhysReg,
							  SmallVectorImpl<unsigned> &SplitVRegs);
		
		void initGraph();
		void simplifyGraph();
		static char ID;
	};
	
	char RAUSCC::ID = 0;
	
} // end anonymous namespace

RAUSCC::RAUSCC(): MachineFunctionPass(ID) {
	initializeLiveDebugVariablesPass(*PassRegistry::getPassRegistry());
	initializeLiveIntervalsPass(*PassRegistry::getPassRegistry());
	initializeSlotIndexesPass(*PassRegistry::getPassRegistry());
	initializeRegisterCoalescerPass(*PassRegistry::getPassRegistry());
	initializeMachineSchedulerPass(*PassRegistry::getPassRegistry());
	initializeLiveStacksPass(*PassRegistry::getPassRegistry());
	initializeMachineDominatorTreePass(*PassRegistry::getPassRegistry());
	initializeMachineLoopInfoPass(*PassRegistry::getPassRegistry());
	initializeVirtRegMapPass(*PassRegistry::getPassRegistry());
	initializeLiveRegMatrixPass(*PassRegistry::getPassRegistry());
}

void RAUSCC::getAnalysisUsage(AnalysisUsage &AU) const {
	AU.setPreservesCFG();
	AU.addRequired<AliasAnalysis>();
	AU.addPreserved<AliasAnalysis>();
	AU.addRequired<LiveIntervals>();
	AU.addPreserved<LiveIntervals>();
	AU.addPreserved<SlotIndexes>();
	AU.addRequired<LiveDebugVariables>();
	AU.addPreserved<LiveDebugVariables>();
	AU.addRequired<LiveStacks>();
	AU.addPreserved<LiveStacks>();
	AU.addRequired<MachineBlockFrequencyInfo>();
	AU.addPreserved<MachineBlockFrequencyInfo>();
	AU.addRequiredID(MachineDominatorsID);
	AU.addPreservedID(MachineDominatorsID);
	AU.addRequired<MachineLoopInfo>();
	AU.addPreserved<MachineLoopInfo>();
	AU.addRequired<VirtRegMap>();
	AU.addPreserved<VirtRegMap>();
	AU.addRequired<LiveRegMatrix>();
	AU.addPreserved<LiveRegMatrix>();
	MachineFunctionPass::getAnalysisUsage(AU);
}

void RAUSCC::releaseMemory() {
	SpillerInstance.reset();
	InterferenceNode* temp = removed.getHead();
	while(removed.isEmpty()) {
	  removed.removeNode(temp);
	  delete temp;
	  temp = removed.getHead();
	}
}


// Spill or split all live virtual registers currently unified under PhysReg
// that interfere with VirtReg. The newly spilled or split live intervals are
// returned by appending them to SplitVRegs.
bool RAUSCC::spillInterferences(LiveInterval &VirtReg, unsigned PhysReg,
								 SmallVectorImpl<unsigned> &SplitVRegs) {
	// Record each interference and determine if all are spillable before mutating
	// either the union or live intervals.
	SmallVector<LiveInterval*, 8> Intfs;
	
	// Collect interferences assigned to any alias of the physical register.
	for (MCRegUnitIterator Units(PhysReg, TRI); Units.isValid(); ++Units) {
		LiveIntervalUnion::Query &Q = Matrix->query(VirtReg, *Units);
		Q.collectInterferingVRegs();
		if (Q.seenUnspillableVReg())
			return false;
		for (unsigned i = Q.interferingVRegs().size(); i; --i) {
			LiveInterval *Intf = Q.interferingVRegs()[i - 1];
			if (!Intf->isSpillable() || Intf->weight > VirtReg.weight)
				return false;
			Intfs.push_back(Intf);
		}
	}
	DEBUG(dbgs() << "spilling " << TRI->getName(PhysReg) <<
		  " interferences with " << VirtReg << "\n");
	assert(!Intfs.empty() && "expected interference");
	std::cout << "Spilling "; std::cout.flush(); VirtReg.dump();
	// Spill each interfering vreg allocated to PhysReg or an alias.
	for (unsigned i = 0, e = Intfs.size(); i != e; ++i) {
		LiveInterval &Spill = *Intfs[i];
		
		// Skip duplicates.
		if (!VRM->hasPhys(Spill.reg))
			continue;
		
		// Deallocate the interfering vreg by removing it from the union.
		// A LiveInterval instance may not be in a union during modification!
		Matrix->unassign(Spill);
		
		// Spill the extracted interval.
		LiveRangeEdit LRE(&Spill, SplitVRegs, *MF, *LIS, VRM);
		spiller().spill(LRE);
	}
	return true;
}

// Driver for the register assignment and splitting heuristics.
// Manages iteration over the LiveIntervalUnions.
//
// This is a minimal implementation of register assignment and splitting that
// spills whenever we run out of registers.
//
// selectOrSplit can only be called once per live virtual register. We then do a
// single interference test for each register the correct class until we find an
// available register. So, the number of interference tests in the worst case is
// |vregs| * |machineregs|. And since the number of interference tests is
// minimal, there is no value in caching them outside the scope of
// selectOrSplit().
unsigned RAUSCC::selectOrSplit(LiveInterval &VirtReg,
								SmallVectorImpl<unsigned> &SplitVRegs) {
	// Populate a list of physical register spill candidates.
	SmallVector<unsigned, 8> PhysRegSpillCands;
	
	// Check for an available register in this class.
	AllocationOrder Order(VirtReg.reg, *VRM, RegClassInfo);
	while (unsigned PhysReg = Order.next()) {
		// Check for interference in PhysReg
		switch (Matrix->checkInterference(VirtReg, PhysReg)) {
			case LiveRegMatrix::IK_Free:
				// PhysReg is available, allocate it.
				std::cout << "Assigning to physical register: "; std::cout.flush(); VirtReg.dump();
				return PhysReg;
				
			case LiveRegMatrix::IK_VirtReg:
				// Only virtual registers in the way, we may be able to spill them.
				PhysRegSpillCands.push_back(PhysReg);
				continue;
				
			default:
				// RegMask or RegUnit interference.
				continue;
		}
	}
	
	// Try to spill another interfering reg with less spill weight.
	for (SmallVectorImpl<unsigned>::iterator PhysRegI = PhysRegSpillCands.begin(),
		 PhysRegE = PhysRegSpillCands.end(); PhysRegI != PhysRegE; ++PhysRegI) {
		if (!spillInterferences(VirtReg, *PhysRegI, SplitVRegs))
			continue;
		
		assert(!Matrix->checkInterference(VirtReg, *PhysRegI) &&
			   "Interference after spill.");
		// Tell the caller to allocate to this newly freed physical register.
		return *PhysRegI;
	}
	
	// No other spill candidates were found, so spill the current VirtReg.
	DEBUG(dbgs() << "spilling: " << VirtReg << '\n');
	std::cout << "Spilling "; std::cout.flush(); VirtReg.dump();
	if (!VirtReg.isSpillable())
		return ~0u;
	LiveRangeEdit LRE(&VirtReg, SplitVRegs, *MF, *LIS, VRM);
	spiller().spill(LRE);
	
	// The live virtual register requesting allocation was spilled, so tell
	// the caller not to allocate anything during this round.
	return 0;
}

bool RAUSCC::runOnMachineFunction(MachineFunction &mf) {
	DEBUG(dbgs() << "********** USCC REGISTER ALLOCATION **********\n"
		  << "********** Function: "
		  << mf.getName() << '\n');
	std::cout << "********** USCC REGISTER ALLOCATION **********\n";
	std::string funcName(mf.getName());
	std::cout << "********** Function: " << funcName << '\n';
	std::cout << "NUM_COLORS=" << NUM_COLORS << '\n';
	MF = &mf;
	RegAllocBase::init(getAnalysis<VirtRegMap>(),
					   getAnalysis<LiveIntervals>(),
					   getAnalysis<LiveRegMatrix>());
	
	calculateSpillWeightsAndHints(*LIS, *MF,
								  getAnalysis<MachineLoopInfo>(),
								  getAnalysis<MachineBlockFrequencyInfo>());
	
	SpillerInstance.reset(createInlineSpiller(*this, *MF, *VRM));
	
	initGraph();
	simplifyGraph();
	
	allocatePhysRegs();
	
	// Diagnostic output before rewriting
	DEBUG(dbgs() << "Post alloc VirtRegMap:\n" << *VRM << "\n");
	
	releaseMemory();
	return true;
}

// Build an interference graph
void RAUSCC::initGraph() {
  InterferenceNode* temp;
  // Add all LiveIntervals to the map and check for overlap
  for(unsigned i = 0, e = MRI->getNumVirtRegs(); i != e; ++i) {
    // Get LiveInterval
    unsigned Reg = TargetRegisterInfo::index2VirtReg(i);
    if(MRI->reg_nodbg_empty(Reg))
      continue;
    LiveInterval* VirtReg = &LIS->getInterval(Reg);

    // Create Node
    InterferenceNode* node = new InterferenceNode();
    node->interval = VirtReg;

    // Go through vector looking for overlaps
    temp = graph.getHead();
    while(temp != nullptr) {
      if(temp->interval->overlaps(*VirtReg)) {
        temp->neighbors.push_back(node);
        node->neighbors.push_back(temp);
        node->neighborCount++;
        temp->neighborCount++;
      }
      temp = temp->next;
    }

    // Push to the graph
    graph.pushBack(node);
  }
}

void RAUSCC::simplifyGraph() {
	// Go through and deconstruct the graph
	InterferenceNode* temp;
	InterferenceNode* tempNext;
	bool needSpill;
	int removalCount = 0;
	while(graph.isEmpty()) {
	  temp = graph.getHead();
	  needSpill = true;
	  // Look for any with neighbors less than NUM_COLORS
	  while(temp != nullptr) {
      tempNext = temp->next;
      if(temp->neighborCount < NUM_COLORS) {
        // Trivially Colorable
        needSpill = false;
        // Add to Queue
        regStack.insert(std::pair<LiveInterval*, unsigned int>(temp->interval, removalCount));
        // Print information
        std::cout << "Found neighbors=" << temp->neighborCount << " for ";
        std::cout.flush();
        temp->interval->dump();
        std::cout << "Removal " << removalCount++ << ": "; std::cout.flush(); temp->interval->dump();
        graph.removeNode(temp);
        for(InterferenceNode* node : temp->neighbors) {
          node->neighborCount--;
        }
        removed.pushBack(temp);
        break;
      }
      temp = tempNext;
	  }
	  if(needSpill) {
	    temp = graph.getHead();
	    InterferenceNode* minNode;
	    float minSpill = MAXFLOAT;
	    // Look for best register to spill
	    while(temp != nullptr) {
	      if(temp->interval->weight < minSpill) {
	        minSpill = temp->interval->weight;
	        minNode = temp;
	      }
	      temp = temp->next;
	    }
      // Add to Queue
      regStack.insert(std::pair<LiveInterval*, unsigned int>(minNode->interval, removalCount));
	    // Spill the minimum weight
	    std::cout << "Spill candidate (neighbors=" << minNode->neighborCount << ", weight=" << minSpill << "): ";
	    std::cout.flush();
	    minNode->interval->dump();
	    std::cout << "Removal " << removalCount++ << ": "; std::cout.flush(); minNode->interval->dump();
	    graph.removeNode(minNode);
      for(InterferenceNode* node : minNode->neighbors) {
        node->neighborCount--;
      }
	    removed.pushBack(minNode);
	  }
	}
}

void InterferenceList::removeNode(InterferenceNode* node) {
  if(node->prev == nullptr) {
    this->head = node->next;
  }
  else {
    node->prev->next = node->next;
  }
  if(node->next == nullptr) {
    this->tail = node->prev;
  }
  else {
    node->next->prev = node->prev;
  }
  node->next = nullptr;
  node->prev = nullptr;
}

void InterferenceList::pushBack(InterferenceNode* node) {
  if(this->tail == nullptr) {
    this->head = node;
    this->tail = node;
  }
  else {
    this->tail->next = node;
    node->prev = this->tail;
    this->tail = node;
  }
}

FunctionPass* createUSCCRegisterAllocator() {
	return new RAUSCC();
}
