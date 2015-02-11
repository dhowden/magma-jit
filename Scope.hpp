// Copyright 2015, David Howden
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef SCOPE_HPP_3FH0MKGQ
#define SCOPE_HPP_3FH0MKGQ

using namespace llvm;

//===----------------------------------------------------------------------===//
// Scope handling: Normal/Loop/Iterative Loop & Function
//===----------------------------------------------------------------------===//

class Scope {
public:
   BasicBlock *EntryBB;
   std::map<std::string, Value*> locals; // keep track of locals (allocas)

   Scope(BasicBlock *entry_bb) : EntryBB(entry_bb) {}

   BasicBlock* getEntryBB() { return EntryBB; }

   virtual bool containsLocal(const std::string &name)
   {
      return (locals.find(name) != locals.end());
   }

   virtual Value* getLocal(const std::string &name) const
   {
      return locals.find(name)->second;
   }

   virtual void addLocal(std::string name, Value *alloca)
   {
      locals[name] = alloca;
   }

   virtual bool isLoopScope() const { return false; }
   virtual bool isIterativeLoopScope() const { return false; }
   virtual bool isFunctionScope() const { return false; }
   virtual bool isLookAheadFunctionScope() const { return false; }

   /// check to see if you can assign a local with this name
   virtual bool canAssignLocal(const std::string &name) const { return true; }

   /// check to see if you can move up to the next scope to assign a local
   /// of this name
   virtual bool canAssignLocalInHigherScope(const std::string &name) const { return true; }

   /// register an imported variable with given name
   virtual void registerImport(const std::string &name) {}

   /// provide a state output for the scope
   virtual void dump(std::ostream &out) const
   {
      dumpName(out);
      out << "{" << std::endl << "  Locals:" << std::endl;
      dumpLocals(out);
      dumpOthers(out);
      out << "}" << std::endl;
   }

   /// the scope name
   virtual void dumpName(std::ostream &out) const
   {
      out << "General Scope";
   }

   /// the scope locals
   virtual void dumpLocals(std::ostream &out) const
   {
      std::map<std::string, Value*>::const_iterator it;
      for (it = locals.begin(); it != locals.end(); ++it)
         out << "    " << it->first << " : " << it->second << std::endl;
   }

   /// other information
   virtual void dumpOthers(std::ostream &out) const {}
};

/// Used in 'while-do' and 'repeat-until' (and 'for' - see IterativeLoopScope)
/// - forces allocs to be registered in a higher scope
/// - provides BreakBB for jumping to on break
class LoopScope : public Scope {
public:
   BasicBlock *BreakBB;
   LoopScope(BasicBlock *entry_bb, BasicBlock *break_bb)
      : Scope(entry_bb), BreakBB(break_bb) {}

   virtual bool isLoopScope() const { return true; }

   /// don't allow any locals to be assigned in the loop scope
   virtual bool canAssignLocal(const std::string &name) const { return false; }

   /// only allow locals which aren't the same name as the loop var be assigned
   virtual bool canAssignLocalInHigherScope(const std::string &name) const { return true; }

   virtual void dumpName(std::ostream &out) const
   {
      out << "Loop Scope";
   }
};

/// Used in for loops
/// - prevents overwriting of loop variable
class IterativeLoopScope : public LoopScope {
public:
   std::string LoopVariable;
   IterativeLoopScope(const std::string &loop_variable, BasicBlock *entry_bb, BasicBlock *break_bb)
      : LoopVariable(loop_variable), LoopScope(entry_bb, break_bb) {}

   virtual bool isIterativeLoopScope() const { return true; }

   /// only allow locals which aren't the same name as the loop var be assigned
   virtual bool canAssignLocalInHigherScope(const std::string &name) const { return (name != LoopVariable); }

   virtual void dumpName(std::ostream &out) const
   {
      out << "Iterative Loop Scope (" << LoopVariable << ")";
   }
};

typedef std::set<std::string> StringSet;

/// Used in functions/procedures
class FunctionScope : public Scope {
public:
   StringSet Imports;
   bool isRecursive;
   std::string FunctionName;

   FunctionScope(const std::string &function_name, BasicBlock *entry_bb)
      : FunctionName(function_name), Scope(entry_bb), isRecursive(false) {}

   FunctionScope(const std::string &function_name, BasicBlock *entry_bb, StringSet imports,
      bool is_recursive) : FunctionName(function_name), Scope(entry_bb),
      Imports(imports), isRecursive(is_recursive) {}

   virtual bool isFunctionScope() const { return true; }

   /// locals can be assigned, but you can't override the function name or
   /// imports from outside the function, but you can override indirect imports
   virtual bool canAssignLocal(const std::string &name) const
   {
      return (name != FunctionName) && (Imports.find(name) == Imports.end());
   }

   /// all locals cannot be assigned outside the function scope
   virtual bool canAssignLocalInHigherScope(const std::string &name) const { return false; }

   virtual void dumpName(std::ostream &out) const
   {
      out << "Function Scope (" << FunctionName << ")";
   }

   virtual void dumpOthers(std::ostream &out) const
   {
      out << "  Imports:" << std::endl;
      _dumpStringSet(Imports, out);
   }

   void _dumpStringSet(const StringSet &set, std::ostream &out) const
   {
      StringSet::const_iterator it;
      for (it = set.begin(); it != set.end(); ++it)
         out << "    " << *it << std::endl;
   }
};

class LookAheadFunctionScope : public FunctionScope {
public:
   LookAheadFunctionScope(const std::string &function_name)
      : FunctionScope(function_name, NULL) {}

   /// Slight modification here to allow for recursive functions
   virtual bool containsLocal(const std::string &name)
   {
      if (FunctionName == name) {
         isRecursive = true;
         return true;
      }
      return Scope::containsLocal(name);
   }

   /// This is also a function scope...?
   virtual bool isLookAheadFunctionScope() const { return true; }

   /// registering variables that are used from outside the function scope
   virtual void registerImport(const std::string &name) { Imports.insert(name); }

   virtual void dumpName(std::ostream &out) const
   {
      out << "LookAheadFunctionScope (" << FunctionName << ")";
   }
};

#endif /* end of include guard: SCOPE_HPP_3FH0MKGQ */
