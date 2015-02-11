// Copyright 2015, David Howden
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef CONTEXT_HPP_VPRQ6RI8
#define CONTEXT_HPP_VPRQ6RI8

using namespace llvm;

extern cl::opt<bool> EmitScopeDump;

//===----------------------------------------------------------------------===//
// Context class: manages scopes
//===----------------------------------------------------------------------===//

typedef std::list<Scope *> ScopeList;
typedef std::queue<LookAheadFunctionScope *> LookAheadFunctionScopeQueue;

class Context {
public:
   std::map<std::string, Value *> internalStringConstants;
   std::map<std::string, std::string *> internalSymbols;

   ScopeList scopes;
   LookAheadFunctionScopeQueue lookAheadQueue;

   Context() {}
   virtual ~Context() {}

   //===-------------------------------------------------------------------===//
   // Internal contants/symbols handling
   //===-------------------------------------------------------------------===//

   Value* getInternalStringConstant(const std::string &name)
   {
      std::map<std::string, Value *>::iterator it = internalStringConstants.find(name);
      if (it == internalStringConstants.end())
         return NULL;
      return it->second;
   }

   void registerInternalStringConstant(const std::string &name, Value *value)
   {
      internalStringConstants.insert(std::pair<std::string, Value*>(name, value));
   }

   std::string* getInternalSymbol(const std::string &name)
   {
      std::map<std::string, std::string*>::iterator it = internalSymbols.find(name);
      if (it == internalSymbols.end())
         return NULL;
      return it->second;
   }

   void registerInternalSymbol(const std::string &name, std::string *symbol)
   {
      internalSymbols.insert(std::pair<std::string, std::string*>(name, symbol));
   }

   //===-------------------------------------------------------------------===//
   // Scope 'stack' handling
   //===-------------------------------------------------------------------===//

   Scope* currentScope() { return scopes.back(); }
   BasicBlock *currentEntryBlock() { return scopes.back()->EntryBB; }

   void createScope(BasicBlock *entry_block)
   {
      scopes.push_back(new Scope(entry_block));
   }

   void createLoopScope(BasicBlock *entry_block, BasicBlock *break_block)
   {
      scopes.push_back(new LoopScope(entry_block, break_block));
   }

   void createIterativeLoopScope(const std::string &loop_variable, BasicBlock *entry_block, BasicBlock *break_block)
   {
      scopes.push_back(new IterativeLoopScope(loop_variable, entry_block, break_block));
   }

   FunctionScope* createFunctionScope(const std::string &function_name, BasicBlock *entry_block)
   {
      LookAheadFunctionScope *lookAhead = lookAheadQueue.front();
      if (function_name != lookAhead->FunctionName)
         Error::exit("Look ahead error, expected function '" + lookAhead->FunctionName
            +"' got '" + function_name + "'");
      FunctionScope *functionScope = new FunctionScope(function_name, entry_block,
         lookAhead->Imports, lookAhead->isRecursive);
      scopes.push_back(functionScope);
      lookAheadQueue.pop();
      return functionScope;
   }

   void createLookAheadFunctionScope(const std::string &function_name)
   {
      LookAheadFunctionScope *lookAheadFunctionScope
         = new LookAheadFunctionScope(function_name);
      scopes.push_back(lookAheadFunctionScope);
      lookAheadQueue.push(lookAheadFunctionScope);
   }

   void popScope()
   {
      Scope *top = scopes.back();
      scopes.pop_back();
      if (EmitScopeDump)
         top->dump(std::cerr);
      if (!top->isLookAheadFunctionScope())
         delete top;
   }

   //===-------------------------------------------------------------------===//
   // Find scope types by iterating through stack
   //===-------------------------------------------------------------------===//

   LoopScope* getLoopScope()
   {
      ScopeList::reverse_iterator it = scopes.rbegin();
      while(it != scopes.rend() && !((*it)->isLoopScope()))
         ++it;

      if (it != scopes.rend()) {
         return static_cast<LoopScope *>((*it));
      }
      return NULL;
   }

   IterativeLoopScope* getIterativeLoopScope(const std::string &identifierName)
   {
      ScopeList::reverse_iterator it = scopes.rbegin();
      while(it != scopes.rend() && (!(*it)->isIterativeLoopScope() || !(*it)->containsLocal(identifierName)))
         ++it;

      if (it != scopes.rend()) {
         return static_cast<IterativeLoopScope *>((*it));
      }
      return NULL;
   }

   FunctionScope* getFunctionScope()
   {
      ScopeList::reverse_iterator it = scopes.rbegin();
      while(it != scopes.rend() && !((*it)->isFunctionScope()))
         ++it;

      if (it != scopes.rend()) {
         return static_cast<FunctionScope *>((*it));
      }
      return NULL;
   }

   LookAheadFunctionScope* getLookAheadFunctionScope()
   {
      ScopeList::reverse_iterator it = scopes.rbegin();
      while(it != scopes.rend() && !((*it)->isLookAheadFunctionScope()))
         ++it;

      if (it != scopes.rend()) {
         return static_cast<LookAheadFunctionScope *>((*it));
      }
      return NULL;
   }

   //===-------------------------------------------------------------------===//
   // Scoped alloca handling
   //===-------------------------------------------------------------------===//

   Value* getLocal(const std::string &name)
   {
      ScopeList::reverse_iterator it = scopes.rbegin();
      while(it != scopes.rend() && !((*it)->containsLocal(name)))
         ++it;

      if (it != scopes.rend())
         return (*it)->getLocal(name);

      return NULL;
   }

   /// Gets an Alloca for loading (reading)
   Value* getLocalForLoad(const std::string &name)
   {
      ScopeList::reverse_iterator it = scopes.rbegin();
      bool lookAheadMode = false;
      while (it != scopes.rend() && !((*it)->containsLocal(name))) {
         /// check if we need to register the import (if found!)
         if ((*it)->isLookAheadFunctionScope())
            lookAheadMode = true;
         ++it;
      }

      if (it == scopes.rend())
         return NULL;

      Value *Alloca = (*it)->getLocal(name);

      /// In look-ahead mode we need to go back and register imports
      if (lookAheadMode) {
         ScopeList::reverse_iterator it2 = scopes.rbegin();
         for (it2 = scopes.rbegin(); it2 != it; ++it2)
            if ((*it2)->isLookAheadFunctionScope())
               (*it2)->registerImport(name);
      }

      return Alloca;
   }

   /// Find the first scope that we can assign a variable with this name in
   Scope* getScopeContainingLocalForAssignment(const std::string &name)
   {
      ScopeList::reverse_iterator it = scopes.rbegin();
      while(it != scopes.rend() && !((*it)->containsLocal(name)) && (*it)->canAssignLocalInHigherScope(name))
         ++it;

      if (it != scopes.rend() && (*it)->containsLocal(name))
         return (*it);

      return NULL;
   }

   /// Find the first scope that we can create a new variable for assignment
   Scope* getScopeForCreatingLocalAssignment(const std::string &name)
   {
      /// the variable doesn't already exist, so can we create it?
      ScopeList::reverse_iterator it = scopes.rbegin();
      while (it != scopes.rend() && !(*it)->canAssignLocal(name) && (*it)->canAssignLocalInHigherScope(name))
         ++it;

      if (it != scopes.rend() && (*it)->canAssignLocal(name))
         return (*it);

      return NULL;
   }
};

#endif /* end of include guard: CONTEXT_HPP_VPRQ6RI8 */
