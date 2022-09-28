//
//  DeadBlocks.cpp
//  uscc
//
//  Implements Dead Block Removal optimization pass.
//  This removes blocks from the CFG which are unreachable.
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
#include <llvm/IR/CFG.h>
#include <llvm/ADT/DepthFirstIterator.h>
#pragma clang diagnostic pop
#include <set>

using namespace llvm;

namespace uscc
{
namespace opt
{
	
bool DeadBlocks::runOnFunction(Function& F)
{
	bool changed = false;
	
	// Created a visited set of BasicBlocks
	std::set<BasicBlock*> visitedSet;
	std::set<BasicBlock*> unreachableSet;

	// Peform DFS from entry block
	for(df_ext_iterator<BasicBlock*, std::set<BasicBlock*>> it = df_ext_begin(&F.getEntryBlock(), visitedSet);
		it != df_ext_end(&F.getEntryBlock(), visitedSet); it++) {}
	
	// Go over every basic block
	Function::iterator blockIter = F.begin();
	while(blockIter != F.end()) {
		if(visitedSet.find(blockIter) == visitedSet.end()) {
			unreachableSet.insert(blockIter);
		}
		blockIter++;
	}

	// Go over all basic blocks in unreachable set
	if(unreachableSet.size() > 0) {
		changed = true;
		for(std::set<BasicBlock*>::iterator it = unreachableSet.begin();
			it != unreachableSet.end(); it++) {
			// Go over all successors
			for(succ_iterator sit = succ_begin(*it); sit != succ_end(*it); sit++) {
				// Tell successor to remove it as predecessor
					(*sit)->removePredecessor(*it);
			}
			(*it)->eraseFromParent();
		}
	}

	return changed;
}
	
void DeadBlocks::getAnalysisUsage(AnalysisUsage& Info) const
{
	// Require ConstantBranch
	Info.addRequired<ConstantBranch>();
}

} // opt
} // uscc

char uscc::opt::DeadBlocks::ID = 0;
