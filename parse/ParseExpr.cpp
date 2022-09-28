//
//  ParseExpr.cpp
//  uscc
//
//  Implements all of the recursive descent parsing
//  functions for the expression grammar rules.
//
//---------------------------------------------------------
//  Copyright (c) 2014, Sanjay Madhav
//  All rights reserved.
//
//  This file is distributed under the BSD license.
//  See LICENSE.TXT for details.
//---------------------------------------------------------

#include "Parse.h"
#include "Symbols.h"
#include <iostream>
#include <sstream>

using namespace uscc::parse;
using namespace uscc::scan;

using std::shared_ptr;
using std::make_shared;

shared_ptr<ASTExpr> Parser::parseExpr()
{
	shared_ptr<ASTExpr> retVal;
	
	// We should first get a AndTerm
	shared_ptr<ASTExpr> andTerm = parseAndTerm();
	
	// If we didn't get an andTerm, then this isn't an Expr
	if (andTerm)
	{
		retVal = andTerm;
		// Check if this is followed by an op (optional)
		shared_ptr<ASTLogicalOr> exprPrime = parseExprPrime(retVal);
		
		if (exprPrime)
		{
			// If we got a exprPrime, return this instead of just term
			retVal = exprPrime;
		}
	}
	
	return retVal;
}

shared_ptr<ASTLogicalOr> Parser::parseExprPrime(shared_ptr<ASTExpr> lhs)
{
	shared_ptr<ASTLogicalOr> retVal;
	
	// Must be ||
	int col = mColNumber;
	if (peekToken() == Token::Or)
	{
		// Make the binary cmp op
		Token::Tokens op = peekToken();
		retVal = make_shared<ASTLogicalOr>();
		consumeToken();
		
		// Set the lhs to our parameter
		retVal->setLHS(lhs);
		
		// We MUST get a AndTerm as the RHS of this operand
		shared_ptr<ASTExpr> rhs = parseAndTerm();
		if (!rhs)
		{
			throw OperandMissing(op);
		}
		
		retVal->setRHS(rhs);
		
		// Check types
		if(!retVal->finalizeOp()) {
			std::string msg = "Cannot perform op between type ";
			msg += std::string(getTypeText(lhs->getType()));
			msg += " and ";
			msg += std::string(getTypeText(rhs->getType()));
			reportSemantError(msg, col);
		}
		
		// See comment in parseTermPrime if you're confused by this
		shared_ptr<ASTLogicalOr> exprPrime = parseExprPrime(retVal);
		if (exprPrime)
		{
			retVal = exprPrime;
		}
	}
	
	return retVal;
}

// AndTerm -->
shared_ptr<ASTExpr> Parser::parseAndTerm()
{
	shared_ptr<ASTExpr> retVal;
	shared_ptr<ASTExpr> tempRelExpr;

	// Check for RelExpr
	if((tempRelExpr = parseRelExpr())) {
		// Check for &&
		if(peekToken() == Token::And) {
			retVal = parseAndTermPrime(tempRelExpr);
		} else {
			retVal = tempRelExpr;
		}
	}
	
	return retVal;
}

shared_ptr<ASTLogicalAnd> Parser::parseAndTermPrime(shared_ptr<ASTExpr> lhs)
{
	shared_ptr<ASTLogicalAnd> retVal;
	shared_ptr<ASTLogicalAnd> tempOp = make_shared<ASTLogicalAnd>();
	shared_ptr<ASTExpr> tempRelExpr;
	tempOp->setLHS(lhs);

	// Get the RelExpr
	int col = mColNumber;
	consumeToken();
	try {
		if(!(tempRelExpr = parseRelExpr())) {
			throw OperandMissing(Token::And);
		}
	} catch (ParseExceptMsg &e) {
		reportError(e);

		// consume until semicolon
		consumeUntil(Token::SemiColon);
		if(peekToken() == Token::EndOfFile) {
			throw EOFExcept();
		}
		consumeToken();
		tempOp->setRHS(make_shared<ASTBadExpr>());
		return tempOp;
	}

	// Check for AndTerm'
	tempOp->setRHS(tempRelExpr);

	// Check types
	if(!tempOp->finalizeOp()) {
		std::string msg = "Cannot perform op between type ";
		msg += std::string(getTypeText(lhs->getType()));
		msg += " and ";
		msg += std::string(getTypeText(tempRelExpr->getType()));
		reportSemantError(msg, col);
	}

	if(peekToken() == Token::And) {
		retVal = parseAndTermPrime(tempOp);
	} else {
		retVal = tempOp;
	}

	return retVal;
}

// RelExpr -->
shared_ptr<ASTExpr> Parser::parseRelExpr()
{
	shared_ptr<ASTExpr> retVal;
	shared_ptr<ASTExpr> tempNumExpr;

	//check for NumExpr
	if((tempNumExpr = parseNumExpr())) {
		//check for operator
		if(peekIsOneOf({Token::EqualTo, Token::NotEqual, Token::LessThan, Token::GreaterThan})) {
			retVal = parseRelExprPrime(tempNumExpr);
		} else {
			retVal = tempNumExpr;
		}
	}
	
	return retVal;
}

shared_ptr<ASTBinaryCmpOp> Parser::parseRelExprPrime(shared_ptr<ASTExpr> lhs)
{
	shared_ptr<ASTBinaryCmpOp> retVal;
	shared_ptr<ASTBinaryCmpOp> tempOp;
	shared_ptr<ASTExpr> tempNumExpr;

	// get operator
	Token::Tokens op = peekToken();
	int col = mColNumber;
	tempOp = make_shared<ASTBinaryCmpOp>(op);
	tempOp->setLHS(lhs);
	consumeToken();

	// get the NumExpr
	try {
		if(!(tempNumExpr = parseNumExpr())) {
			throw OperandMissing(op);
		}
	} catch (ParseExceptMsg &e) {
		reportError(e);

		// consume until semicolon
		consumeUntil(Token::SemiColon);
		if(peekToken() == Token::EndOfFile) {
			throw EOFExcept();
		}
		consumeToken();
		tempOp->setRHS(make_shared<ASTBadExpr>());
		return tempOp;
	}

	//Check for RelExpr'
	tempOp->setRHS(tempNumExpr);

	// Check types
	// Check types
	if(!tempOp->finalizeOp()) {
		std::string msg = "Cannot perform op between type ";
		msg += std::string(getTypeText(lhs->getType()));
		msg += " and ";
		msg += std::string(getTypeText(tempNumExpr->getType()));
		reportSemantError(msg, col);
	}

	if(peekIsOneOf({Token::EqualTo, Token::NotEqual, Token::LessThan, Token::GreaterThan})) {
		retVal = parseRelExprPrime(tempOp);
	} else {
		retVal = tempOp;
	}

	

	
	return retVal;
}

// NumExpr -->
shared_ptr<ASTExpr> Parser::parseNumExpr()
{
	shared_ptr<ASTExpr> retVal;
	shared_ptr<ASTExpr> tempTerm;

	//Check for Term
	if((tempTerm = parseTerm())) {
		// Check for NumExpr'
		if(peekIsOneOf({Token::Plus, Token::Minus})) {
			retVal = parseNumExprPrime(tempTerm);
		} else {
			retVal = tempTerm;
		}
	}

	return retVal;
}

shared_ptr<ASTBinaryMathOp> Parser::parseNumExprPrime(shared_ptr<ASTExpr> lhs)
{
	shared_ptr<ASTBinaryMathOp> retVal;
	shared_ptr<ASTBinaryMathOp> tempOp;
	shared_ptr<ASTExpr> tempTerm;
	
	// Get operator
	Token::Tokens op = peekToken();
	int col = mColNumber;
	tempOp = make_shared<ASTBinaryMathOp>(op);
	tempOp->setLHS(lhs);
	consumeToken();

	// Get the term
	try {
		tempTerm = parseTerm();
		if(!tempTerm) {
			throw OperandMissing(op);
		}
	} catch (ParseExceptMsg &e) {
		reportError(e);

		// consume until semicolon
		consumeUntil(Token::SemiColon);
		if(peekToken() == Token::EndOfFile) {
			throw EOFExcept();
		}
		consumeToken();
		tempOp->setRHS(make_shared<ASTBadExpr>());
		return tempOp;
	}

	// Check for NumExprPrime
	tempOp->setRHS(tempTerm);

	// Check types
	if(!tempOp->finalizeOp()) {
		std::string msg = "Cannot perform op between type ";
		msg += std::string(getTypeText(lhs->getType()));
		msg += " and ";
		msg += std::string(getTypeText(tempTerm->getType()));
		reportSemantError(msg, col);
	}

	if(peekIsOneOf({Token::Plus, Token::Minus})) {
		retVal = parseNumExprPrime(tempOp);
	} else {
		retVal = tempOp;
	}

	return retVal;
}

// Term -->
shared_ptr<ASTExpr> Parser::parseTerm()
{
	shared_ptr<ASTExpr> retVal;
	shared_ptr<ASTExpr> tempVal;

	// check for Value
	if((tempVal = parseValue())) {
		// check for term'
		if(peekIsOneOf({Token::Mult, Token::Div, Token::Mod})) {
			retVal = parseTermPrime(tempVal);
		} else {
			retVal = tempVal;
		}
	}
	
	return retVal;
}

shared_ptr<ASTBinaryMathOp> Parser::parseTermPrime(shared_ptr<ASTExpr> lhs)
{
	shared_ptr<ASTBinaryMathOp> retVal;
	shared_ptr<ASTBinaryMathOp> tempOp;
	shared_ptr<ASTExpr> tempVal;

	// get operator
	Token::Tokens op = peekToken();
	int col = mColNumber;
	tempOp = make_shared<ASTBinaryMathOp>(op);
	tempOp->setLHS(lhs);
	consumeToken();

	//get the value
	try {
		tempVal = parseValue();
		if(!tempVal) {
			throw OperandMissing(op);
		}
	} catch (ParseExceptMsg &e) {
		reportError(e);

		// consume until semicolon
		consumeUntil(Token::SemiColon);
		if(peekToken() == Token::EndOfFile) {
			throw EOFExcept();
		}
		consumeToken();
		tempOp->setRHS(make_shared<ASTBadExpr>());
		return tempOp;
	}
	
	// check to see if it is a term prime
	tempOp->setRHS(tempVal);
	
	// Check types
	if(!tempOp->finalizeOp()) {
		std::string msg = "Cannot perform op between type ";
		msg += std::string(getTypeText(lhs->getType()));
		msg += " and ";
		msg += std::string(getTypeText(tempVal->getType()));
		reportSemantError(msg, col);
	}

	if(peekIsOneOf({Token::Mult, Token::Div, Token::Mod})) {
		retVal = parseTermPrime(tempOp);
	} else {
		retVal = tempOp;
	}
	
	return retVal;
}

// Value -->
shared_ptr<ASTExpr> Parser::parseValue()
{
	shared_ptr<ASTExpr> retVal;
	shared_ptr<ASTExpr> notFactor;
	
	// check for ! sign
	if(peekToken() == Token::Not) {
		consumeToken();
		// parse the following factor
		try {
			if(!(notFactor = parseFactor())) {
				throw ParseExceptMsg("! must be followed by an expression.");
			}
		} catch (ParseExceptMsg &e) {
			reportError(e);

			//consume and make a bad expression
			notFactor = make_shared<ASTBadExpr>();
		}
		retVal = make_shared<ASTNotExpr>(notFactor);
	} else {
		retVal = parseFactor();
	}
	
	return retVal;
}

// Factor -->
shared_ptr<ASTExpr> Parser::parseFactor()
{
	shared_ptr<ASTExpr> retVal;
	
	// Try parse identifier factors FIRST so
	// we make sure to consume the mUnusedIdents
	// before we try any other rules
	
	if ((retVal = parseIdentFactor()))
		;
	else if ((retVal = parseConstantFactor()))
		;
	else if ((retVal = parseStringFactor()))
		;
	else if ((retVal = parseParenFactor()))
		;
	else if ((retVal = parseDecFactor()))
		;
	else if ((retVal = parseIncFactor()))
		;
	else if ((retVal = parseAddrOfArrayFactor()))
		;
	
	return retVal;
}

// ( Expr )
shared_ptr<ASTExpr> Parser::parseParenFactor()
{
	shared_ptr<ASTExpr> retVal;

	// Check for parentheses
	if(peekToken() == Token::LParen){
		// Consume it
		consumeToken();

		// get the expression
		try{
			//Check for expression
			if(!(retVal = parseExpr())) {
				throw ParseExceptMsg("Not a valid expression inside parenthesis");
			}
		}
		catch (ParseExceptMsg& e) {
			// Exrror with expression
			reportError(e);
			// skip to end of return
			consumeUntil(Token::RParen);
			if(peekToken() == Token::EndOfFile){
				throw EOFExcept();
			}
			consumeToken();
			return make_shared<ASTBadExpr>();
		}

		// Consume the RBrace
		matchToken(Token::RParen);
	}
	
	return retVal;
}

// constant
shared_ptr<ASTConstantExpr> Parser::parseConstantFactor()
{
	shared_ptr<ASTConstantExpr> retVal;

	// Check if it is a constant
	if(peekToken() == Token::Constant) {
		// Retrieve the constant
		const char* constStr = getTokenTxt();

		// Make the ASTNode
		retVal = make_shared<ASTConstantExpr>(constStr);

		// Consume the token
		consumeToken();
	}
	
	return retVal;
}

// string
shared_ptr<ASTStringExpr> Parser::parseStringFactor()
{
	shared_ptr<ASTStringExpr> retVal;

	// check if it is a string
	if(peekToken() == Token::String) {
		// Retrieve the string
		const char* constStr = getTokenTxt();

		// Make the node
		retVal = make_shared<ASTStringExpr>(std::string(constStr), mStrings);

		// Consume the token
		consumeToken();
	}
	
	return retVal;
}

// id
// id [ Expr ]
// id ( FuncCallArgs )
shared_ptr<ASTExpr> Parser::parseIdentFactor()
{
	shared_ptr<ASTExpr> retVal;
	if (peekToken() == Token::Identifier ||
		mUnusedIdent != nullptr || mUnusedArray != nullptr)
	{
		if (mUnusedArray)
		{
			// "unused array" means that AssignStmt looked at this array
			// and decided it didn't want it, so it's already made an
			// array sub node
			retVal = make_shared<ASTArrayExpr>(mUnusedArray);
			mUnusedArray = nullptr;
		}
		else
		{
			Identifier* ident = nullptr;
			
			// If we have an "unused identifier," which means that
			// AssignStmt looked at this and decided it didn't want it,
			// that means we're already a token AFTER the identifier.
			if (mUnusedIdent)
			{
				ident = mUnusedIdent;
				mUnusedIdent = nullptr;
			}
			else
			{
				ident = getVariable(getTokenTxt());
				consumeToken();
			}
			
			// Now we need to look ahead and see if this is an array
			// or function call reference, since id is a common
			// left prefix.
			if (peekToken() == Token::LBracket)
			{
				// Check to make sure this is an array
				if (mCheckSemant && ident->getType() != Type::IntArray &&
					ident->getType() != Type::CharArray &&
					!ident->isDummy())
				{
					std::string err("'");
					err += ident->getName();
					err += "' is not an array";
					reportSemantError(err);
					consumeUntil(Token::RBracket);
					if (peekToken() == Token::EndOfFile)
					{
						throw EOFExcept();
					}
					
					matchToken(Token::RBracket);
					
					// Just return our error variable
					retVal = make_shared<ASTIdentExpr>(*mSymbols.getIdentifier("@@variable"));
				}
				else
				{
					consumeToken();
					try
					{
						shared_ptr<ASTExpr> expr = parseExpr();
						if (!expr)
						{
							throw ParseExceptMsg("Valid expression required inside [ ].");
						}
						
						shared_ptr<ASTArraySub> array = make_shared<ASTArraySub>(*ident, expr);
						retVal = make_shared<ASTArrayExpr>(array);
					}
					catch (ParseExcept& e)
					{
						// If this expr is bad, consume until RBracket
						reportError(e);
						consumeUntil(Token::RBracket);
						if (peekToken() == Token::EndOfFile)
						{
							throw EOFExcept();
						}
					}
					
					matchToken(Token::RBracket);
				}
			}
			else if (peekToken() == Token::LParen)
			{
				// Check to make sure this is a function
				if (mCheckSemant && ident->getType() != Type::Function &&
					!ident->isDummy())
				{
					std::string err("'");
					err += ident->getName();
					err += "' is not a function";
					reportSemantError(err);
					consumeUntil(Token::RParen);
					if (peekToken() == Token::EndOfFile)
					{
						throw EOFExcept();
					}
					
					matchToken(Token::RParen);
					
					// Just return our error variable
					retVal = make_shared<ASTIdentExpr>(*mSymbols.getIdentifier("@@variable"));
				}
				else
				{
					consumeToken();
					// A function call can have zero or more arguments
					shared_ptr<ASTFuncExpr> funcCall = make_shared<ASTFuncExpr>(*ident);
					retVal = funcCall;
					
					// Get the number of arguments for this function
					shared_ptr<ASTFunction> func = ident->getFunction();
					
					try
					{
						int currArg = 1;
						int col = mColNumber;
						shared_ptr<ASTExpr> arg = parseExpr();
						while (arg)
						{
							// Check for validity of this argument (for non-dummy functions)
							if (!ident->isDummy())
							{
								// Special case for "printf" since we don't make a node for it
								if (ident->getName() == "printf")
								{
									mNeedPrintf = true;
									if (currArg == 1 && arg->getType() != Type::CharArray)
									{
										reportSemantError("The first parameter to printf must be a char[]");
									}
								}
								else if (mCheckSemant)
								{
									if (currArg > func->getNumArgs())
									{
										std::string err("Function ");
										err += ident->getName();
										err += " takes only ";
										std::ostringstream ss;
										ss << func->getNumArgs();
										err += ss.str();
										err += " arguments";
										reportSemantError(err, col);
									}
									else if (!func->checkArgType(currArg, arg->getType()))
									{
										// If we have an int and the expected arg type is a char,
										// we can do a conversion
										if (arg->getType() == Type::Int &&
											func->getArgType(currArg) == Type::Char)
										{
											arg = intToChar(arg);
										}
										else
										{
											std::string err("Expected expression of type ");
											err += getTypeText(func->getArgType(currArg));
											reportSemantError(err, col);
										}
									}
								}
							}
							
							funcCall->addArg(arg);
							
							currArg++;
							
							if (peekAndConsume(Token::Comma))
							{
								col = mColNumber;
								arg = parseExpr();
								if (!arg)
								{
									throw
									ParseExceptMsg("Comma must be followed by expression in function call");
								}
							}
							else
							{
								break;
							}
						}
					}
					catch (ParseExcept& e)
					{
						reportError(e);
						consumeUntil(Token::RParen);
						if (peekToken() == Token::EndOfFile)
						{
							throw EOFExcept();
						}
					}
					
					// Now make sure we have the correct number of arguments
					if (!ident->isDummy())
					{
						// Special case for printf
						if (ident->getName() == "printf")
						{
							if (funcCall->getNumArgs() == 0)
							{
								reportSemantError("printf requires a minimum of one argument");
							}
						}
						else if (mCheckSemant && funcCall->getNumArgs() < func->getNumArgs())
						{
							std::string err("Function ");
							err += ident->getName();
							err += " requires ";
							std::ostringstream ss;
							ss << func->getNumArgs();
							err += ss.str();
							err += " arguments";
							reportSemantError(err);
						}
					}
					
					matchToken(Token::RParen);
				}
			}
			else
			{
				// Just a plain old ident
				retVal = make_shared<ASTIdentExpr>(*ident);
			}
		}
	}

	return charToInt(retVal);
}

// ++ id
shared_ptr<ASTExpr> Parser::parseIncFactor()
{
	shared_ptr<ASTExpr> retVal;
	
	// get the increment operator
	if(peekToken() == Token::Inc){
		// consume
		consumeToken();

		// get the identifier
		try{
			if(peekToken() == Token::Identifier) {
				const char* id = getTokenTxt();
				if(!mSymbols.getIdentifier(id)) {
					std::string msg = "Use of undeclared identifier \'" + std::string(id) + "\'";
					reportSemantError(msg);
					retVal = make_shared<ASTIncExpr>(*(mSymbols.getIdentifier("@@variable")));
				} else {
					retVal = make_shared<ASTIncExpr>(*(mSymbols.getIdentifier(id)));
				}
				consumeToken();
			} else {
				throw ParseExceptMsg("++ must be followed by an identifier.");
			}
		} catch (ParseExceptMsg &e) {
			reportError(e);

			// Consume until semi-colon
			consumeUntil(Token::SemiColon);
			if(peekToken() == Token::EndOfFile) {
				throw EOFExcept();
			}

			// Consume the ;
			consumeToken();
		}
	}
	
	return charToInt(retVal);
}

// -- id
shared_ptr<ASTExpr> Parser::parseDecFactor()
{
	shared_ptr<ASTExpr> retVal;
	
	// get the decrement operator
	if(peekToken() == Token::Dec){
		// consume
		consumeToken();

		// get the identifier
		try{
			if(peekToken() == Token::Identifier) {
				const char* id = getTokenTxt();
				if(!mSymbols.getIdentifier(id)) {
					std::string msg = "Use of undeclared identifier \'" + std::string(id) + "\'";
					reportSemantError(msg);
					retVal = make_shared<ASTDecExpr>(*(mSymbols.getIdentifier("@@variable")));
				} else {
					retVal = make_shared<ASTDecExpr>(*(mSymbols.getIdentifier(id)));
				}
				consumeToken();
			} else {
				throw ParseExceptMsg("-- must be followed by an identifier.");
			}
		} catch (ParseExceptMsg &e) {
			reportError(e);

			// Consume until semi-colon
			consumeUntil(Token::SemiColon);
			if(peekToken() == Token::EndOfFile) {
				throw EOFExcept();
			}

			// Consume the ;
			consumeToken();
		}
	}

	return charToInt(retVal);
}

// & id [ Expr ]
shared_ptr<ASTExpr> Parser::parseAddrOfArrayFactor()
{
	shared_ptr<ASTExpr> retVal;
	shared_ptr<ASTExpr> tempExpr;
	
	// get the & sign
	if(peekToken() == Token::Addr) {
		consumeToken();

		// get the id [ Expr ]
		try{
			// get the identifier
			if(peekToken() != Token::Identifier){
				throw ParseExceptMsg("& must be followed by an identifier.");
			}
			const char* id = getTokenTxt();
			Identifier *ident = mSymbols.getIdentifier(id);
			consumeToken();

			// get the brace
			matchToken(Token::LBracket);

			// get the expression
			if(!(tempExpr = parseExpr())) {
				throw ParseExceptMsg("Missing required subscript expression.");
			}

			// get the ]
			matchToken(Token::RBracket);

			// create retVal
			retVal = make_shared<ASTAddrOfArray>(make_shared<ASTArraySub>(*ident, tempExpr));
		} catch (ParseExceptMsg &e) {
			reportError(e);
			// Consume until semi-colon
			consumeUntil(Token::SemiColon);
			if(peekToken() == Token::EndOfFile) {
				throw EOFExcept();
			}
			retVal = make_shared<ASTBadExpr>();
		}
	}
	
	return retVal;
}
