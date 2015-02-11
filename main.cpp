// Copyright 2015, David Howden
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <iostream>
#include <string>
#include <cstdio>

#include "AST.hpp"
#include "Printer.hpp"
#include "CodeGen.hpp"

extern int yyparse();
extern BlockAST *programBlock;
extern FILE* yyin;
extern std::string const* filename;

cl::list<std::string> InputFilenames(cl::Positional, cl::desc("<input files>"), cl::OneOrMore);

cl::opt<bool> OnlyParse("P", cl::desc("Only parse input"));
cl::opt<bool> OnlyCompile("C", cl::desc("Only parse and compile input"));
cl::opt<bool> EmitAST("emit-ast", cl::desc("Emit AST parse tree"));
cl::opt<bool> EmitLLVM("emit-llvm", cl::desc("Emit unoptimised LLVM IR"));
cl::opt<bool> EmitLLVMOpt("emit-llvm-opt", cl::desc("Emit optimised LLVM IR"));
cl::opt<bool> SkipVerify("skip-verify", cl::desc("Skip verification of IR code"));
cl::opt<bool> EmitScopeDump("emit-scope-dump", cl::desc("Emit the scope dump"));

cl::opt<bool> EnabledTailEliminationPass("tail-elim", cl::desc("Enable tail elimintation optimisation pass"));

int main (int argc, char const *argv[])
{
   cl::ParseCommandLineOptions(argc, argv, "Magma JIT Compiler (Prototype)\n");
   if (!InputFilenames.size()) {
      std::cerr << "error: no input files" << std::endl;
      return 1;
   }

   cl::list<std::string>::const_iterator it;
   for (it = InputFilenames.begin(); it != InputFilenames.end(); ++it) {
      yyin = std::fopen(it->c_str(), "r");
      if (yyin == NULL) {
         std::cerr << "Error: '" << *it << "' cannot be opened"
            << std::endl;
         return 1;
      }
      filename = &(*it);
      yyparse();
      std::fclose(yyin);

      if (programBlock) {
         if (EmitAST) {
            Printer p(std::cout);
            programBlock->accept(p);
            p.nl();
         }

         if (OnlyParse)
            return 0;

         CodeGen c;
         c.generateCode(*programBlock);

         if (OnlyCompile)
            return 0;

         c.runCode();
      } else {
         return 1;
      }
   }

   return 0;
}