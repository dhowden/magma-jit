// Copyright 2015, David Howden
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef AST_HPP_LJ5Q56OP
#define AST_HPP_LJ5Q56OP

#include <iostream>
#include <list>

#include "Error.hpp"

// Used in the parser (which includes this file directly).
extern "C" {
typedef struct YYLTYPE {
   int first_line;
   int first_column;
   int last_line;
   int last_column;
   std::string *filename;
} YYLTYPE;
#define YYLTYPE_IS_DECLARED 1 /* alert the parser that we have own defn */
}

#define ASTLOC(var) var->setParseLocation(yyloc, linebuf)
#define ESC_CHAR (char)27

class StatementAST;
class ExpressionAST;
class Visitor;
class IterativeStatementAST;
class IdentifierAST;
class TypeAST;
class ReturnStatementAST;

typedef std::list<StatementAST*> StatementList;
typedef std::list<ExpressionAST*> ExpressionList;
typedef std::list<IterativeStatementAST *> IterativeStatementList;
typedef std::list<IdentifierAST *> IdentifierList;
typedef std::list<TypeAST *> TypeList;

// NodeAST: Root node of AST
class NodeAST {
public:
   YYLTYPE parseLocation;
   std::string parseContext;
   bool parseLocationSet;

   NodeAST() : parseLocationSet(false) {}
   virtual ~NodeAST() {}
   virtual void accept(Visitor &v) const = 0;

   /// For recording parse data for giving meaningful errors!
   virtual void setParseLocation(YYLTYPE &loc, char linebuf[500]);
   /// Make that meaningful error message!
   virtual void compilerError(std::ostream &out, std::string msg) const;
};

/// Block - a list of statements
class BlockAST : public NodeAST {
public:
   StatementList statements;
   bool IsReachable;
   BlockAST() : IsReachable(true) {}
   void addStatement (StatementAST *statement);
   virtual void accept(Visitor &v) const;
};

/// TypeAST - class for parsing types
class TypeAST : public NodeAST {
public:
   std::string Name;
   TypeList *ExtendedTypeList;
   TypeAST(const std::string &name) : Name(name), ExtendedTypeList(NULL) {}
   TypeAST(const std::string &name, TypeList *extended_type_list) : Name(name),
      ExtendedTypeList(extended_type_list) {}
   virtual void accept(Visitor &v) const;
};

/// StatementAST - base class for all statements
class StatementAST : public NodeAST {
public:
   virtual void accept(Visitor &v) const = 0;
   virtual bool makesUnreachable() { return false; }
};

/// ExpressionAST - base class for all expressions
class ExpressionAST : public StatementAST {
public:
   // virtual ~ExpressionAST() {}
   virtual void accept(Visitor &v) const = 0;
};

/// IdentifierAST - class for referencing variables
class IdentifierAST : public ExpressionAST {
public:
   std::string Name;
   TypeAST *Type;
   bool IsReference;
   IdentifierAST(const std::string &name) : Name(name), IsReference(false),
      Type(NULL) {}
   IdentifierAST(const std::string &name, bool is_reference)
      : Name(name), IsReference(is_reference), Type(NULL) {}
   IdentifierAST(const std::string &name, bool is_reference, TypeAST *type)
      : Name(name), IsReference(is_reference), Type(type) {}
   virtual void accept(Visitor &v) const;
};

/// IntegerAST - class for numeric values
class IntegerAST : public ExpressionAST {
public:
   unsigned long Value;
   unsigned int  Size;
   IntegerAST(unsigned long value, unsigned int size = 64)
      : Value(value), Size(size) {}
   virtual void accept(Visitor &v) const;
};

class BooleanAST : public IntegerAST {
public:
   BooleanAST(bool value) : IntegerAST((unsigned long)value, 1) {}
   virtual void accept(Visitor &v) const;
};

class SequenceAST : public ExpressionAST {
public:
   ExpressionList *Expressions;
   SequenceAST(ExpressionList *expressions) : Expressions(expressions) {};
   virtual void accept(Visitor &v) const;
};

class TupleAST : public ExpressionAST {
public:
   ExpressionList *Expressions;
   TupleAST(ExpressionList *expressions) : Expressions(expressions) {};
   virtual void accept(Visitor &v) const;
};

class IndexLookupAST : public ExpressionAST {
public:
   ExpressionAST *Expression, *IndexExpression;
   IndexLookupAST(ExpressionAST *expression, ExpressionAST *index_expression)
      : Expression(expression), IndexExpression(index_expression) {}
   virtual void accept(Visitor &c) const;
};

/// BinaryExpressionAST - class for binary expressions
class BinaryExpressionAST : public ExpressionAST {
public:
   int Op;
   ExpressionAST *LHS, *RHS;

   BinaryExpressionAST(int op, ExpressionAST *lhs, ExpressionAST *rhs)
      : Op(op), LHS(lhs), RHS(rhs) {}
   virtual void accept(Visitor &v) const;
};

/// AssignmentStatementAST
class AssignmentStatementAST : public StatementAST {
public:
   IdentifierAST *Identifier;
   ExpressionAST *Expression;
   ExpressionAST *Index;

   AssignmentStatementAST(IdentifierAST *identifier, ExpressionAST *expression)
      : Identifier(identifier), Expression(expression), Index(NULL) {}
   AssignmentStatementAST(IdentifierAST *identifier, ExpressionAST *expression,
      ExpressionAST *index) : Identifier(identifier), Expression(expression), Index(index) {}
   virtual void accept(Visitor &v) const;
};

/// StringExpressionAST
class StringExpressionAST : public ExpressionAST {
public:
   std::string Value;
   StringExpressionAST(const std::string &value)
      : Value(collapseEscapedChars(value.substr(1, value.size()-2))) {}
   virtual void accept(Visitor &v) const;

   static std::string collapseEscapedChars(const std::string &input);
};

/// PrintStatementAST
class PrintStatementAST : public StatementAST {
public:
   ExpressionList *Expressions;
   PrintStatementAST(ExpressionList *expressions) : Expressions(expressions) {}
   virtual void accept(Visitor &v) const;
};

/// PrintfStatementAST
class PrintfStatementAST : public PrintStatementAST {
public:
   StringExpressionAST *StringExpression;
   PrintfStatementAST(StringExpressionAST *stringExpression, ExpressionList *expressions)
      : PrintStatementAST(expressions), StringExpression(stringExpression) {}
   virtual void accept(Visitor &v) const;
};

/// TimeStatementAST
class TimeStatementAST : public StatementAST {
public:
   StatementAST *Statement;
   TimeStatementAST(StatementAST *statement) : Statement(statement) {}
   virtual void accept(Visitor &v) const;
};

/// AssertStatementAst
class AssertStatementAST : public StatementAST {
public:
   ExpressionAST *Expression;
   unsigned long LineNumber;
   AssertStatementAST(ExpressionAST *expression, unsigned long line_number)
      : Expression(expression), LineNumber(line_number) {}
   virtual void accept(Visitor &v) const;
};

/// Conditional statements

/// If statements
class IfStatementAST : public StatementAST {
public:
   ExpressionAST *Condition;
   BlockAST *Then;
   BlockAST *Else;

   IfStatementAST(ExpressionAST *condition, BlockAST *then, BlockAST *_else)
      : Condition(condition), Then(then), Else(_else) {}
   virtual void accept(Visitor &v) const;
};


/// While statements
class WhileStatementAST : public StatementAST {
public:
   ExpressionAST *Condition;
   BlockAST *Do;
   WhileStatementAST(ExpressionAST *condition, BlockAST *_do)
      : Condition(condition), Do(_do) {}
   virtual void accept(Visitor &v) const;
};

/// Repeat statements (upsidedown while!)
class RepeatStatementAST : public StatementAST {
public:
   BlockAST *Repeat;
   ExpressionAST *Until;
   RepeatStatementAST(BlockAST *repeat, ExpressionAST *until)
      : Repeat(repeat), Until(until) {}
   virtual void accept(Visitor &v) const;
};

/// Iterative statements
class IterativeStatementAST : public StatementAST {
public:
   IdentifierAST *Identifier, *IterableIdentifier;
   ExpressionAST *Begin, *End, *Step;
   IterativeStatementAST *NextStatement;
   BlockAST *Body;
   IterativeStatementAST(IdentifierAST *identifier, ExpressionAST *begin, ExpressionAST *end, ExpressionAST *step)
      : Identifier(identifier), IterableIdentifier(NULL), Begin(begin), End(end), Step(step), NextStatement(NULL), Body(NULL) {}
   IterativeStatementAST(IdentifierAST *identifier, IdentifierAST *iterableIdentifier)
      : Identifier(identifier), IterableIdentifier(iterableIdentifier), Begin(NULL), End(NULL), Step(NULL), NextStatement(NULL), Body(NULL) {}
   virtual void accept(Visitor &v) const;
};

class IterativeStatementsAST : public StatementAST {
   IterativeStatementList IterativeStatements;
public:
   IterativeStatementsAST() {}
   virtual void accept(Visitor &v) const;

   void addIterativeStatement(IterativeStatementAST *statement)
   {
      if (IterativeStatements.size())
         IterativeStatements.back()->NextStatement = statement;
      IterativeStatements.push_back(statement);
   }

   void setBody(BlockAST *body)
   {
      IterativeStatements.back()->Body = body;
   }

   IterativeStatementAST *firstIterativeStatement() const
   {
      return IterativeStatements.front();
   }
};

/// break statement handling
class BreakStatementAST : public StatementAST {
public:
   IdentifierAST *Identifier;
   BreakStatementAST(IdentifierAST *identifier) : Identifier(identifier) {}
   BreakStatementAST() : Identifier(NULL) {}

   virtual void accept(Visitor &v) const;
   virtual bool makesUnreachable() { return true; }
};

/// EvaluatedSequenceAST - class for evalutated sequences
class EvaluatedSequenceAST : public ExpressionAST {
public:
   ExpressionAST *EvaluationExpression;
   IterativeStatementsAST *IterativeStatements;
   ExpressionAST *Condition;
   EvaluatedSequenceAST(ExpressionAST *evalutation_expression,
      IterativeStatementsAST *iterative_statements, ExpressionAST *condition)
      : EvaluationExpression(evalutation_expression), IterativeStatements(iterative_statements), Condition(condition) {}
   virtual void accept(Visitor &v) const;
};


/// Function declaration statement
class FunctionDeclarationStatementAST : public StatementAST {
public:
   IdentifierAST *Name;
   IdentifierList *Arguments;
   BlockAST *Body;
   bool IsProcedure;
   FunctionDeclarationStatementAST(IdentifierAST *name, IdentifierList *arguments, BlockAST *body, bool isProcedure)
      : Name(name), Arguments(arguments), Body(body), IsProcedure(isProcedure) {}
   virtual void accept(Visitor &v) const;
};

class ReturnStatementAST : public StatementAST {
public:
   ExpressionAST *Expression;
   ReturnStatementAST(ExpressionAST *expression) : Expression(expression) {}
   ReturnStatementAST() : Expression(NULL) {}

   virtual void accept(Visitor &v) const;
   virtual bool makesUnreachable() { return true; }
};

class CallExpressionAST : public ExpressionAST {
public:
   IdentifierAST *Name;
   ExpressionList *Arguments;
   CallExpressionAST(IdentifierAST *name, ExpressionList *arguments)
      : Name(name), Arguments(arguments) {}
   virtual void accept(Visitor &v) const;
};


#endif /* end of include guard: AST_HPP_LJ5Q56OP */