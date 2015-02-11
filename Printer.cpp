// Copyright 2015, David Howden
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "Printer.hpp"

void Printer::nl()
{
   Stream << std::endl;
   for (unsigned int i = 0; i < indentLevel; i++)
      Stream << INDENT_UNIT;
}

void Printer::increaseIndent()
{
   indentLevel++;
}

void Printer::decreaseIndent()
{
   if (indentLevel > 0)
      indentLevel--;
}

void Printer::visit(const BlockAST &block)
{
   Stream << "BlockAST { ";
   increaseIndent();
   StatementList::const_iterator it;
   for (it = block.statements.begin(); it != block.statements.end(); ++it) {
      nl();
      (*it)->accept(*this);
   }
   decreaseIndent(); nl();
   Stream << "}";
}

void Printer::visit(const IntegerAST &integer)
{
   Stream << "IntegerAST {" << integer.Value << "}";
}

void Printer::visit(const BooleanAST &boolean)
{
   Stream << "BooleanAST {" << (boolean.Value == 1 ? "true" : "false") << "}";
}

void Printer::visit(const SequenceAST &sequence)
{
   Stream << "SequenceAST {";
   increaseIndent();
   ExpressionList::const_iterator it;
   for (it = sequence.Expressions->begin(); it != sequence.Expressions->end(); ++it) {
      nl(); (*it)->accept(*this);
   }
   decreaseIndent(); nl();
   Stream << "}";
}

void Printer::visit(const TupleAST &tuple)
{
   Stream << "TupleAST {";
   increaseIndent();
   ExpressionList::const_iterator it;
   for (it = tuple.Expressions->begin(); it != tuple.Expressions->end(); ++it) {
      nl(); (*it)->accept(*this);
   }
   decreaseIndent(); nl();
   Stream << "}";
}

void Printer::visit(const IndexLookupAST &index_lookup)
{
   Stream << "IndexLookupAST {";
   increaseIndent(); nl();
   index_lookup.Expression->accept(*this);
   index_lookup.IndexExpression->accept(*this);
   decreaseIndent(); nl();
   Stream << "}";
}

void Printer::visit(const IdentifierAST &identifier)
{
   Stream << "IdentifierAST {" << identifier.Name;
   if (identifier.IsReference)
      Stream << " (ref)";
   if (identifier.Type) {
      Stream << ", ";
      identifier.Type->accept(*this);
   }
   Stream << "}";
}

void Printer::visit(const TypeAST &type)
{
   Stream << "TypeAST {" << type.Name;
   if (type.ExtendedTypeList) {
      Stream << "[";
      TypeList::const_iterator it;
      for (it = type.ExtendedTypeList->begin(); it != type.ExtendedTypeList->end(); ++it)
         (*it)->accept(*this);
      Stream << "]";
   }
   Stream << "}";
}

void Printer::visit(const BinaryExpressionAST &binary_expression)
{
   Stream << "BinaryExpressionAST { ";
   increaseIndent(); nl();
   Stream << "Op='" << binary_expression.Op << "'";
   nl();
   Stream << "RHS:";
   binary_expression.RHS->accept(*this);
   if (binary_expression.LHS != NULL) {
      nl();
      Stream << "LHS:";
      binary_expression.LHS->accept(*this);
   }
   decreaseIndent(); nl();
   Stream << "}";
}

void Printer::visit(const AssignmentStatementAST &assignment_statement)
{
   Stream << "AssignmentStatementAST {";
   increaseIndent(); nl();
   assignment_statement.Identifier->accept(*this); nl();
   assignment_statement.Expression->accept(*this);
   decreaseIndent(); nl();
   Stream << "}";
}

void Printer::visit(const StringExpressionAST &string_expression)
{
   Stream << "String {\"" << string_expression.Value << "\"}";
}

void Printer::visit(const PrintStatementAST &print_statement)
{
   Stream << "PrintStatementAST {";
   increaseIndent();
   ExpressionList::const_iterator it;
   for (it = print_statement.Expressions->begin(); it != print_statement.Expressions->end(); ++it) {
      nl(); (*it)->accept(*this);
   }
   decreaseIndent(); nl();
   Stream << "}";
}

void Printer::visit(const PrintfStatementAST &print_statement)
{
   Stream << "PrintfStatementAST {";
   increaseIndent(); nl();
   Stream << "StringExpression: ";
   print_statement.StringExpression->accept(*this);
   if (print_statement.Expressions->size() > 0) {
      nl();
      Stream << "Expressions: ";
      increaseIndent();
      ExpressionList::const_iterator it;
      for (it = print_statement.Expressions->begin(); it != print_statement.Expressions->end(); ++it) {
         nl(); (*it)->accept(*this);
      }
      decreaseIndent();
   }
   decreaseIndent(); nl();
   Stream << "}";
}

void Printer::visit(const TimeStatementAST &time_statement)
{
   Stream << "TimeStatementAST {";
   increaseIndent(); nl();
   time_statement.Statement->accept(*this);
   decreaseIndent(); nl();
   Stream << "}";
}

void Printer::visit(const AssertStatementAST &assert_statement)
{
   Stream << "AssertStatementAST {";
   increaseIndent(); nl();
   assert_statement.Expression->accept(*this);
   decreaseIndent(); nl();
   Stream << "}";
}

void Printer::visit(const IfStatementAST &if_statement)
{
   Stream << "IfStatementAST {";
   increaseIndent(); nl();
   if_statement.Condition->accept(*this);
   nl();
   if_statement.Then->accept(*this);
   if (if_statement.Else != NULL) {
      nl();
      if_statement.Else->accept(*this);
   }
   decreaseIndent(); nl();
   Stream << "}";
}

void Printer::visit(const WhileStatementAST &while_statement)
{
   Stream << "WhileStatementAST {";
   increaseIndent(); nl();
   while_statement.Condition->accept(*this);
   while_statement.Do->accept(*this);
   decreaseIndent(); nl();
   Stream << "}";
}

void Printer::visit(const RepeatStatementAST &repeat_statement)
{
   Stream << "RepeatStatementAST {";
   increaseIndent(); nl();
   repeat_statement.Repeat->accept(*this);
   repeat_statement.Until->accept(*this);
   decreaseIndent(); nl();
   Stream << "}";
}

void Printer::visit(const IterativeStatementsAST &iterative_statements)
{
   Stream << "IterativeStatementsAST {";
   increaseIndent(); nl();
   iterative_statements.firstIterativeStatement()->accept(*this);
   decreaseIndent(); nl();
   Stream << "}";
}

void Printer::visit(const IterativeStatementAST &iterative_statement)
{
   Stream << "IterativeStatementAST {";
   increaseIndent(); nl();
   Stream << "Ident: ";
   iterative_statement.Identifier->accept(*this);
   nl();
   Stream << "Begin: ";
   iterative_statement.Begin->accept(*this);
   nl();
   Stream << "End:   ";
   iterative_statement.End->accept(*this);
   nl();
   Stream << "Step:  ";
   iterative_statement.Step->accept(*this);
   if (iterative_statement.NextStatement != NULL) {
      nl();
      Stream << "Next Statement: ";
      increaseIndent(); nl();
      iterative_statement.NextStatement->accept(*this);
      decreaseIndent();
   }
   if (iterative_statement.Body != NULL) {
      nl();
      Stream << "Body:";
      increaseIndent(); nl();
      iterative_statement.Body->accept(*this);
      decreaseIndent();
   }
   decreaseIndent(); nl();
   Stream << "}";
}

void Printer::visit(const EvaluatedSequenceAST &evaluated_sequence)
{
   Stream << "EvaluatedSequenceAST {";
   increaseIndent(); nl();
   evaluated_sequence.EvaluationExpression->accept(*this); nl();
   evaluated_sequence.IterativeStatements->accept(*this);
   decreaseIndent(); nl();
   Stream << "}";
}

void Printer::visit(const BreakStatementAST &break_statement)
{
   Stream << "BreakStatementAST {";
   if (break_statement.Identifier)
      break_statement.Identifier->accept(*this);
   Stream << "}";
}

void Printer::visit(const FunctionDeclarationStatementAST &func_decl_statement)
{
   Stream << "FunctionDeclarationStatementAST {";
   increaseIndent(); nl();
   Stream << "IsProcedure: " << func_decl_statement.IsProcedure; nl();
   Stream << "Name: ";
   func_decl_statement.Name->accept(*this);
   nl();
   Stream << "Arguments: ";
   increaseIndent();
   IdentifierList::const_iterator it;
   for (it = func_decl_statement.Arguments->begin(); it != func_decl_statement.Arguments->end(); ++it) {
      nl(); (*it)->accept(*this);
   }
   decreaseIndent(); nl();
   Stream << "Body: ";
   func_decl_statement.Body->accept(*this);
   decreaseIndent(); nl();
   Stream << "}";
}

void Printer::visit(const ReturnStatementAST &return_statement)
{
   Stream << "ReturnStatementAST {";
   if (return_statement.Expression) {
      increaseIndent(); nl();
      return_statement.Expression->accept(*this);
      decreaseIndent(); nl();
   } else {
      Stream << "NULL (void)";
   }
   Stream << "}";
}

void Printer::visit(const CallExpressionAST &call_expression)
{
   Stream << "CallExpressionAST {";
   increaseIndent(); nl();
   Stream << "Name: ";
   call_expression.Name->accept(*this);
   nl();
   Stream << "Arguments: ";
   increaseIndent();
   ExpressionList::const_iterator it;
   for (it = call_expression.Arguments->begin(); it != call_expression.Arguments->end(); ++it) {
      nl(); (*it)->accept(*this);
   }
   decreaseIndent();
   decreaseIndent(); nl();
   Stream << "}";
}