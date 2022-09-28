//
//  LICM.cpp
//  uscc
//
//  Implements basic loop invariant code motion
//
//---------------------------------------------------------
//  Copyright (c) 2014, Sanjay Madhav
//  All rights reserved.
//
//  This file is distributed under the BSD license.
//  See LICENSE.TXT for details.
//---------------------------------------------------------
#include "Passes.h"
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wconversion"
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Dominators.h>
#include <llvm/Analysis/ValueTracking.h>
#pragma clang diagnostic pop

using namespace llvm;

namespace uscc
{
namespace opt
{
	
bool LICM::runOnLoop(llvm::Loop *L, llvm::LPPassManager &LPM)
{
	mChanged = false;
	
	// Save the current loop
	mCurrLoop = L;
	// Grab the loop info
	mLoopInfo = &getAnalysis<LoopInfo>();
	// Get the Dom Tree
	mDomTree = &getAnalysis<DominatorTreeWrapperPass>().getDomTree();

	// Call hoistPreOrder
	hoistPreOrder(mDomTree->getNode(mCurrLoop->getHeader()));
	
	return mChanged;
}

void LICM::getAnalysisUsage(AnalysisUsage &Info) const
{
	// Does not modify CFG
	Info.setPreservesCFG();
	// Execute after dead block removal
	Info.addRequired<DeadBlocks>();
	// Use the build in Dom tree and loop info passes
	Info.addRequired<DominatorTreeWrapperPass>();
	Info.addRequired<LoopInfo>();
}

bool LICM::isSafeToHoistInstr(llvm::Instruction* I) {
	bool safe = false;
	// Check if operands are loop invariant
	if(mCurrLoop->hasLoopInvariantOperands(I)) {
		// Check for side effects
		if(llvm::isSafeToSpeculativelyExecute(I)) {
			// Check class
			if(dyn_cast<BinaryOperator>(I) ||
			   dyn_cast<CastInst>(I) ||
			   dyn_cast<SelectInst>(I) ||
			   dyn_cast<GetElementPtrInst>(I) ||
			   dyn_cast<CmpInst>(I)) {
				safe = true;
			}
		}
	}

	return safe;
}

void LICM::hoistInstr(llvm::Instruction* I) {
	// Get the preheader and terminator instruction
	BasicBlock* preHeader = mCurrLoop->getLoopPreheader();
	Instruction* terminator = preHeader->getTerminator();

	// Move the instruction to before the terminator
	I->moveBefore(terminator);

	// Change mChanged
	mChanged = true;
}

void LICM::hoistPreOrder(llvm::DomTreeNode* node) {
	// Get BasicBlock associated with the node
	BasicBlock* bb = node->getBlock();

	// Is it in the loop
	if(mLoopInfo->getLoopFor(bb) == mCurrLoop) {
		// Go through all instructions
		BasicBlock::iterator instrIter = bb->begin();
		while(instrIter != bb->end()) {
			llvm::Instruction* currInstr = instrIter;
			instrIter++;
			if(isSafeToHoistInstr(currInstr))
				hoistInstr(currInstr);
		}
	}

	// Iterate over all children
	for(DomTreeNode::iterator childIter = node->begin(); childIter != node->end(); childIter++) {
		hoistPreOrder(*childIter);
	}
}

} // opt
} // uscc

char uscc::opt::LICM::ID = 0;
