// Copyright 2015, David Howden
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "ContextLookAhead.hpp"

using namespace llvm;

//===----------------------------------------------------------------------===//
// ContextLookAhead variable handling methods to mirror CodeGen
//===----------------------------------------------------------------------===//

void ContextLookAhead::createLocal(const std::string &name)
{
   return createLocalInScope(name, context.currentScope());
}

void ContextLookAhead::createLocalInScope(const std::string &name, Scope* scope)
{
   /// Don't actually create the alloca in the entry block...
   scope->addLocal(name, NULL);
}

void ContextLookAhead::getOrCreateLocalForAssignment(const std::string &name)
{
   /// See if there is a scope which already has a variable of this name
   /// that we can assign to
   Scope *scope = context.getScopeContainingLocalForAssignment(name);
   if (scope) {
      /// check that we can assign to this name (in this scope)
      if (scope->canAssignLocal(name)) {
         scope->getLocal(name);
      } else {
         std::stringstream str;
         str << "The variable '" << name << "' cannot be overwritten!" << std::endl;
         //scopes.back()->dump(str);
         Error::exit(str.str());
      }
   }

   scope = context.getScopeForCreatingLocalAssignment(name);
   if (!scope)
      Error::exit("could not find scope to assign variable too - internal error :-(");

   createLocalInScope(name, scope);
}

//===----------------------------------------------------------------------===//
// ContextLookAhead Visitor Pattern methods (AST transversal)
//===----------------------------------------------------------------------===//

void ContextLookAhead::visit(const BlockAST &block)
{
   StatementList::const_iterator it;
   for (it = block.statements.begin(); it != block.statements.end(); ++it)
      (*it)->accept(*this);
}

void ContextLookAhead::visit(const SequenceAST &sequence)
{
   ExpressionList::const_iterator it;
   for (it = sequence.Expressions->begin(); it != sequence.Expressions->end(); ++it)
      (*it)->accept(*this);
}

void ContextLookAhead::visit(const TupleAST &tuple)
{
   ExpressionList::const_iterator it;
   for (it = tuple.Expressions->begin(); it != tuple.Expressions->end(); ++it)
      (*it)->accept(*this);
}

void ContextLookAhead::visit(const IndexLookupAST &index_lookup)
{
   index_lookup.Expression->accept(*this);
   index_lookup.IndexExpression->accept(*this);
}

void ContextLookAhead::visit(const BinaryExpressionAST &binary_expression)
{
   binary_expression.RHS->accept(*this);
   if (binary_expression.LHS)
      binary_expression.LHS->accept(*this);
}

void ContextLookAhead::visit(const PrintStatementAST &print_statement)
{
   ExpressionList::const_iterator it;
   for (it = print_statement.Expressions->begin(); it != print_statement.Expressions->end(); ++it)
      (*it)->accept(*this);
}

void ContextLookAhead::visit(const PrintfStatementAST &printf_statement)
{
   /// NB don't need to look at the StringExpression

   if (printf_statement.Expressions) {
      ExpressionList::const_iterator it;
      for (it = printf_statement.Expressions->begin(); it != printf_statement.Expressions->end(); ++it)
         (*it)->accept(*this);
   }
}

void ContextLookAhead::visit(const TimeStatementAST &time_statement)
{
   time_statement.Statement->accept(*this);
}

void ContextLookAhead::visit(const AssertStatementAST &assert_statement)
{
   assert_statement.Expression->accept(*this);
}

void ContextLookAhead::visit(const IfStatementAST &if_statement)
{
   if_statement.Condition->accept(*this);
   if_statement.Then->accept(*this);
   if (if_statement.Else)
      if_statement.Else->accept(*this);
}

void ContextLookAhead::visit(const WhileStatementAST &while_statement)
{
   while_statement.Condition->accept(*this);
   while_statement.Do->accept(*this);
}

void ContextLookAhead::visit(const RepeatStatementAST &repeat_statement)
{
   repeat_statement.Repeat->accept(*this);
   repeat_statement.Until->accept(*this);
}

void ContextLookAhead::visit(const IterativeStatementsAST &iterative_statements)
{
   iterative_statements.firstIterativeStatement()->accept(*this);
}

void ContextLookAhead::visit(const ReturnStatementAST &return_statement)
{
   if (return_statement.Expression)
      return_statement.Expression->accept(*this);
}

void ContextLookAhead::visit(const TypeAST &type) {}
void ContextLookAhead::visit(const IntegerAST &integer) {}
void ContextLookAhead::visit(const BooleanAST &boolean) {}
void ContextLookAhead::visit(const StringExpressionAST &string_expression) {}
void ContextLookAhead::visit(const BreakStatementAST &break_statement) {}

//===----------------------------------------------------------------------===//
// ContextLookAhead Visitor Pattern methods (actually doing something)
//===----------------------------------------------------------------------===//

void ContextLookAhead::visit(const IdentifierAST &identifier)
{
   context.getLocalForLoad(identifier.Name);
}

void ContextLookAhead::visit(const AssignmentStatementAST &assignment_statement)
{
   assignment_statement.Expression->accept(*this);
   if (assignment_statement.Index != NULL)
      assignment_statement.Index->accept(*this);
   getOrCreateLocalForAssignment(assignment_statement.Identifier->Name);
}

void ContextLookAhead::visit(const IterativeStatementAST &iterative_statement)
{
   context.createIterativeLoopScope(iterative_statement.Identifier->Name, NULL, NULL);
   createLocal(iterative_statement.Identifier->Name);
   iterative_statement.Begin->accept(*this);

   if (iterative_statement.NextStatement)
      iterative_statement.NextStatement->accept(*this);
   else if (iterative_statement.Body)
      iterative_statement.Body->accept(*this);

   /// TODO: In CodeGen these are only included if there hasn't been a break
   /// or return.  Check that this is ok!
   iterative_statement.Step->accept(*this);
   iterative_statement.End->accept(*this);

   context.popScope();
}

void ContextLookAhead::visit(const EvaluatedSequenceAST &evaluated_sequence)
{
   return; /// not implemented in CodeGen yet!
}

void ContextLookAhead::visit(const FunctionDeclarationStatementAST &func_decl_statement)
{
   /// Begin function scope
   context.createLookAheadFunctionScope(func_decl_statement.Name->Name);

   /// Declare the function arguments
   IdentifierList::const_iterator IL_it = func_decl_statement.Arguments->begin();
   IdentifierList::const_iterator IL_end = func_decl_statement.Arguments->end();
   for (; IL_it != IL_end; ++IL_it)
      createLocal((*IL_it)->Name);

   /// Go through the function body
   func_decl_statement.Body->accept(*this);

   context.popScope();

   /// Only create a scope variable if we're still in look ahead!
   if (context.getLookAheadFunctionScope())
      getOrCreateLocalForAssignment(func_decl_statement.Name->Name);
}

void ContextLookAhead::visit(const CallExpressionAST &call_expression)
{
   /// Get the function being called
   context.getLocalForLoad(call_expression.Name->Name);

   /// Go through the parameters
   ExpressionList::const_iterator it;
   for (it = call_expression.Arguments->begin(); it != call_expression.Arguments->end(); ++it)
      (*it)->accept(*this);
}