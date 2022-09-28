//
//  SSABuilder.cpp
//  uscc
//
//  Implements SSABuilder class
//  
//---------------------------------------------------------
//  Copyright (c) 2014, Sanjay Madhav
//  All rights reserved.
//
//  This file is distributed under the BSD license.
//  See LICENSE.TXT for details.
//---------------------------------------------------------

#include "SSABuilder.h"
#include "../parse/Symbols.h"

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wconversion"
#include <llvm/IR/Value.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Constants.h>
#pragma clang diagnostic pop

#include <list>
#include <vector>

using namespace uscc::opt;
using namespace uscc::parse;
using namespace llvm;

// To deal with my memory management
SSABuilder::~SSABuilder() {
	reset();
}

// Called when a new function is started to clear out all the data
void SSABuilder::reset()
{
	// Clear mVarDefs
	for(auto const& tempMap : mVarDefs) {
		tempMap.second->clear();
		delete tempMap.second;
	}
	mVarDefs.clear();

	// Clear mIncompletePhis
	for(auto const& tempMap : mIncompletePhis) {
		tempMap.second->clear();
		delete tempMap.second;
	}
	mIncompletePhis.clear();

	// Clear mSealedBlocks
	mSealedBlocks.clear();
}

// For a specific variable in a specific basic block, write its value
void SSABuilder::writeVariable(Identifier* var, BasicBlock* block, Value* value)
{
	if(mVarDefs[block]->find(var) != mVarDefs[block]->end())
		mVarDefs[block]->at(var) = value;
	else
		mVarDefs[block]->insert(std::pair<Identifier*, Value*>(var, value));
}

// Read the value assigned to the variable in the requested basic block
// Will recursively search predecessor blocks if it was not written in this block
Value* SSABuilder::readVariable(Identifier* var, BasicBlock* block)
{
	// Check current block for var
	if(mVarDefs[block]->find(var) != mVarDefs[block]->end()) 
		return mVarDefs[block]->at(var);
	
	return readVariableRecursive(var, block);
}

// This is called to add a new block to the maps
void SSABuilder::addBlock(BasicBlock* block, bool isSealed /* = false */)
{
	// Add block to mVarDefs and mIncompletePhis
	SubMap* subMap = new SubMap();
	SubPHI* subPhi = new SubPHI();

	mVarDefs.insert(std::pair<BasicBlock*, SubMap*>(block, subMap));
	mIncompletePhis.insert(std::pair<BasicBlock*, SubPHI*>(block, subPhi));

	// Seal block if true
	if(isSealed)
		sealBlock(block);
}

// This is called when a block is "sealed" which means it will not have any
// further predecessors added. It will complete any PHI nodes (if necessary)
void SSABuilder::sealBlock(llvm::BasicBlock* block)
{
	for(auto var : *(mIncompletePhis[block])) {
		addPhiOperands(var.first, mIncompletePhis[block]->at(var.first));
	}
	mSealedBlocks.insert(block);
}

// Recursively search predecessor blocks for a variable
Value* SSABuilder::readVariableRecursive(Identifier* var, BasicBlock* block)
{
	Value* retVal = nullptr;
	
	// Check if all predecessors known
	if(mSealedBlocks.find(block) == mSealedBlocks.end()){
		if(block->getFirstNonPHI() == block->end()) {
			// No other instructions
			retVal = PHINode::Create(var->llvmType(), 0, var->getName(), block);
		}
		else {
			retVal = PHINode::Create(var->llvmType(), 0, var->getName(), block->getFirstNonPHI());
		}
		mIncompletePhis[block]->insert(std::pair<Identifier*, PHINode*>(var, (llvm::PHINode*) retVal));
	} 
	else if(block->getSinglePredecessor() != nullptr) {
		// Check if only one predecessor (no phi)
		retVal = readVariable(var, block->getSinglePredecessor());
	}
	else {
		// Phi node is needed
		if(block->getFirstNonPHI() == block->end()) {
			// No other instructions
			retVal = PHINode::Create(var->llvmType(), 0, var->getName(), block);
		}
		else {
			retVal = PHINode::Create(var->llvmType(), 0, var->getName(), block->getFirstNonPHI());
		}
		writeVariable(var, block, retVal);
		retVal = addPhiOperands(var, (llvm::PHINode*)retVal);
	}

	writeVariable(var, block, retVal);	
	return retVal;
}

// Adds phi operands based on predecessors of the containing block
Value* SSABuilder::addPhiOperands(Identifier* var, PHINode* phi)
{
	for(pred_iterator pred=pred_begin(phi->getParent()); pred!=pred_end(phi->getParent()); pred++) {
		phi->addIncoming(readVariable(var, *pred), *pred);
	}
	
	return tryRemoveTrivialPhi(phi);
}

// Removes trivial phi nodes
Value* SSABuilder::tryRemoveTrivialPhi(llvm::PHINode* phi)
{
	Value* same = nullptr;
	std::vector<Value*> users;
	
	// Go over all operands in the Phi node
	int numOperands = phi->getNumIncomingValues();
	for(int i = 0; i < numOperands; i++) {
		Value* op = phi->getIncomingValue(i);
		if (op==same || op==phi)
			continue;
		if (same != nullptr)
			return phi;
		same = op;
	}

	// Check if phi is unreachable
	if(same == nullptr) {
		same = UndefValue::get(phi->getType());
	}

	// Remove the phi from its uses
	for(Value::use_iterator it = phi->use_begin(); it != phi->use_end(); it++) {
		if(*it != phi) {
			users.push_back(*it);
		}
	}

	// Replace all uses with same
	phi->replaceAllUsesWith(same);

	// Replace phi in the variable map
	BasicBlock* bb = phi->getParent();
	for(auto const& var : *mVarDefs[bb]){
		if(var.second == phi) {
			mVarDefs[bb]->at(var.first) = same;
		}
	}

	// Erase phi node from parent
	phi->eraseFromParent();

	// Check other phi nodes
	for(auto use : users) {
		if(dyn_cast<PHINode>(use) != nullptr) {
			tryRemoveTrivialPhi((llvm::PHINode*)use);
		}
	}

	return same;
}
