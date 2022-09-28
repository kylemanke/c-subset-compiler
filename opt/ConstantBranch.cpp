//
//  ContantBranch.cpp
//  uscc
//
//  Implements Constant Branch Folding opt pass.
//  This converts conditional branches on constants to
//  unconditional branches.
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
#pragma clang diagnostic pop
#include <set>

using namespace llvm;

namespace uscc
{
namespace opt
{
	
bool ConstantBranch::runOnFunction(Function& F)
{
	bool changed = false;
	
	// Set for branch instructions to fix
	std::set<BranchInst*> removeSet;

	// Loop through the instructions
	Function::iterator blockIter = F.begin();
	while (blockIter != F.end()) {
		// get the terminator instruction
		Instruction* terminator = blockIter->getTerminator();
		
		// Is it a branch
		if(BranchInst* br = dyn_cast<BranchInst>(terminator)) {
			// Is it conditional and is a ConstantInt
			if(br->isConditional() && dyn_cast<ConstantInt>(br->getCondition())) {
				// Add to remove set
				removeSet.insert(br);
			}
		}
		blockIter++;
	}

	if(removeSet.size() > 0) {
		changed = true;
		std::set<BranchInst*>::iterator i;
		for(i = removeSet.begin(); i != removeSet.end(); i++) {
			BasicBlock* left = (*i)->getSuccessor(0);
			BasicBlock* right = (*i)->getSuccessor(1);
			BasicBlock* bb = (*i)->getParent();

			// Is condition true
			if(dyn_cast<ConstantInt>((*i)->getCondition())->isZero() == false) {
				// Create unconditional branch to left successor
				BranchInst::Create(left, bb);
				// Notify right that bb is no longer parent
				right->removePredecessor(bb);
			}
			// Condition is false
			else { 
				// CreateBr to right 
				BranchInst::Create(right, bb);
				// Notify left that bb is no longer predecessor
				left->removePredecessor(bb);
			}

			// remove old branch
			(*i)->eraseFromParent();
		}
	}
	
	return changed;
}

void ConstantBranch::getAnalysisUsage(AnalysisUsage& Info) const
{
	// Require Constant Propagation
	Info.addRequired<ConstantOps>();
}
	
} // opt
} // uscc

char uscc::opt::ConstantBranch::ID = 0;
