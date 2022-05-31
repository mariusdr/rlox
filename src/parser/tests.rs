#![allow(dead_code)]
use super::*;

#[cfg(test)] 
mod parser_tests {
    use super::*;

    fn not_expr() -> Expr {
        let x = make_false_literal();
        make_unaryop(UnaryOpType::Not, x)
    }
    fn not_stmt() -> Stmt { make_expr(not_expr()) }
    
    fn neg_expr() -> Expr {
        let x = make_num_literal("123");
        make_unaryop(UnaryOpType::Neg, x)
    }
    fn neg_stmt() -> Stmt { make_expr(neg_expr()) }

    #[test]
    fn simple_unary_expr() {
        let src = "!false; -123;";
        let mut parser = Parser::new(src);
        let stmts = parser.parse().unwrap();
        assert_eq!(stmts[0], not_stmt());
        assert_eq!(stmts[1], neg_stmt());
    }

    fn true_stmt() -> Stmt { make_expr(make_true_literal()) }
    fn false_stmt() -> Stmt { make_expr(make_false_literal()) }
    fn nil_stmt() -> Stmt { make_expr(make_nil_literal()) }
    fn int_stmt() -> Stmt { make_expr(make_num_literal("123.456")) }
    fn str_stmt() -> Stmt { make_expr(make_str_literal("foobar")) }

    #[test]
    fn parse_simple_literals() {
        let src = "true; false; nil; 123.456;";
        let mut parser = Parser::new(src);
        let stmts = parser.parse().unwrap();
        assert_eq!(stmts[0], true_stmt());
        assert_eq!(stmts[1], false_stmt());
        assert_eq!(stmts[2], nil_stmt());
        assert_eq!(stmts[3], int_stmt());
    }

    #[test]
    fn parse_string_literal() {
        let src = "\"foobar\";";
        let mut parser = Parser::new(src);
        let stmts = parser.parse().unwrap();
        assert_eq!(stmts[0], str_stmt());
    }
    
    #[test]
    fn string_literal_missing_closer() {
        let src = "\"foobar;";
        let mut parser = Parser::new(src);
        let res = parser.parse();
        assert!(res.is_err());
    }

    fn sum_expr() -> Expr {
        let num1 = make_num_literal("1");
        let num2 = make_num_literal("2");
        make_binaryop(BinaryOpType::Add, num1, num2)
    }
    fn sum_stmt() -> Stmt { make_expr(sum_expr()) }

    fn sub_expr() -> Expr {
        let num1 = make_num_literal("1");
        let num2 = make_num_literal("2");
        make_binaryop(BinaryOpType::Sub, num1, num2)
    }
    fn sub_stmt() -> Stmt { make_expr(sub_expr()) }

    fn mult_expr() -> Expr {
        let num1 = make_num_literal("1");
        let num2 = make_num_literal("2");
        make_binaryop(BinaryOpType::Mult, num1, num2)
    }
    fn mult_stmt() -> Stmt { make_expr(mult_expr()) }
    
    fn div_expr() -> Expr {
        let num1 = make_num_literal("1");
        let num2 = make_num_literal("2");
        make_binaryop(BinaryOpType::Div, num1, num2)
    }
    fn div_stmt() -> Stmt { make_expr(div_expr()) }

    fn gt_expr() -> Expr {
        let x = make_num_literal("1");
        let y = make_num_literal("2");
        make_binaryop(BinaryOpType::Greater, x, y)
    }
    fn gt_stmt() -> Stmt { make_expr(gt_expr()) }
    
    fn lt_expr() -> Expr {
        let x = make_num_literal("1");
        let y = make_num_literal("2");
        make_binaryop(BinaryOpType::Less, x, y)
    }
    fn lt_stmt() -> Stmt { make_expr(lt_expr()) }
    
    fn ge_expr() -> Expr {
        let x = make_num_literal("1");
        let y = make_num_literal("2");
        make_binaryop(BinaryOpType::GreaterEqual, x, y)
    }
    fn ge_stmt() -> Stmt { make_expr(ge_expr()) }
    
    fn le_expr() -> Expr {
        let x = make_num_literal("1");
        let y = make_num_literal("2");
        make_binaryop(BinaryOpType::LessEqual, x, y)
    }
    fn le_stmt() -> Stmt { make_expr(le_expr()) }

    fn eq_expr() -> Expr {
        let x = make_num_literal("1");
        let y = make_num_literal("2");
        make_binaryop(BinaryOpType::Equal, x, y)
    }
    fn eq_stmt() -> Stmt { make_expr(eq_expr()) }
    
    fn ne_expr() -> Expr {
        let x = make_num_literal("1");
        let y = make_num_literal("2");
        make_binaryop(BinaryOpType::NotEqual, x, y)
    }
    fn ne_stmt() -> Stmt { make_expr(ne_expr()) }

    #[test]
    fn simple_binop_expr() {
        let src = " 1+2; 1-2; 1*2; 1/2; \
                    1>2; 1>=2; 1<2; 1<=2; \
                    1==2; 1!=2;";

        let mut parser = Parser::new(src);
        let stmts = parser.parse().unwrap();
        assert_eq!(stmts[0], sum_stmt());
        assert_eq!(stmts[1], sub_stmt());
        assert_eq!(stmts[2], mult_stmt());
        assert_eq!(stmts[3], div_stmt());
        assert_eq!(stmts[4], gt_stmt());
        assert_eq!(stmts[5], ge_stmt());
        assert_eq!(stmts[6], lt_stmt());
        assert_eq!(stmts[7], le_stmt());
        assert_eq!(stmts[8], eq_stmt());
        assert_eq!(stmts[9], ne_stmt());
    }

    #[test]
    fn subtracting_negative_number() {
        let src = "1--2;";
        let res = Parser::new(src).parse().unwrap();
        let s = make_expr(
            make_binaryop(BinaryOpType::Sub, 
                make_num_literal("1"), 
                make_unaryop(UnaryOpType::Neg, 
                    make_num_literal("2"))));
        assert_eq!(res[0], s);
    }

    #[test]
    fn missing_semicolon() {
        let src = "1+2+3-4*7";
        let mut parser = Parser::new(src);
        let res = parser.parse();
        assert!(res.is_err());
    }

    fn grp_expr() -> Expr {
        let g1 = make_grouping(sum_expr());
        let g2 = make_grouping(sub_expr());
        let g3 = make_grouping(mult_expr());
        let s = make_binaryop(BinaryOpType::Add, g2, g3);
        let g4 = make_grouping(s);
        make_binaryop(BinaryOpType::Sub, g1, g4)
    }
    fn grp_stmt() -> Stmt {
        make_expr(grp_expr())
    }

    #[test]
    fn complex_expr() {
        let src = "(1+2) - ((1-2) + (1*2));";
        let res = Parser::new(src).parse().unwrap();
        assert_eq!(res[0], grp_stmt());
    }

    fn vardecl_stmt() -> Stmt {
        make_vardecl("x", Some(grp_expr()))
    }

    #[test]
    fn vardecl_with_init() {
        let src = "var x = (1+2) - ((1-2) + (1*2));";
        let res = Parser::new(src).parse().unwrap();
        assert_eq!(res[0], vardecl_stmt());
    }

    fn varsum_expr() -> Expr {
        make_binaryop(BinaryOpType::Add, make_variable("x"), make_variable("y"))
    }
    fn varsum_stmt() -> Stmt { 
        make_expr(varsum_expr())
    }

    #[test]
    fn variable_statement() {
        let src = "zoidberg;";
        let res = Parser::new(src).parse().unwrap();
        let v = make_expr(make_variable("zoidberg"));
        assert_eq!(res[0], v);

        let src = "x+y;";
        let res = Parser::new(src).parse().unwrap();
        assert_eq!(res[0], varsum_stmt());
    }

    fn varassign_expr() -> Expr {
        make_assign("x", grp_expr())
    }
    fn varassign_stmt() -> Stmt { make_expr(varassign_expr()) }

    #[test]
    fn varassign() {
        let src = "x = (1+2) - ((1-2) + (1*2));";
        let res = Parser::new(src).parse().unwrap();
        assert_eq!(res[0], varassign_stmt());
    }
    
    #[test]
    fn varassign_missing_semicolon() {
        let src = "x = (1+2) - ((1-2) + (1*2))";
        let res = Parser::new(src).parse();
        assert!(res.is_err());
    }

    #[test]
    fn vardecl_without_init() {
        let src = "var x;";
        let res = Parser::new(src).parse().unwrap();
        let vd = make_vardecl("x", None);
        assert_eq!(res[0], vd);
    }
    
    #[test]
    fn vardecl_missing_semicolon() {
        let src = "var x";
        let res = Parser::new(src).parse();
        assert!(res.is_err());
    }

    #[test]
    fn print_statement() {
        let src = "print 1+2;";
        let res = Parser::new(src).parse().unwrap();
        let v = make_print(sum_expr()); 
        assert_eq!(res[0], v);
    }

    #[test]
    fn block_statement() {
        let src = "{ 1+2; 1-2; 1*2; }";
        let res = Parser::new(src).parse().unwrap();
        let v = make_block(vec![sum_stmt(), sub_stmt(), mult_stmt()]);
        assert_eq!(res[0], v);
    }

    #[test]
    fn nested_blocks() {
        let src = "{{{ 1+2; } 1-2; } 1*2; }";
        let res = Parser::new(src).parse().unwrap();
        let v = make_block(vec![
            make_block(vec![
                make_block(vec![
                    sum_stmt(),
                ]),
                sub_stmt(),
            ]),
            mult_stmt()
        ]);
        assert_eq!(res[0], v);
    }

    #[test]
    fn if_statement() {
        let src = "if (1==2) 1+2; else 1-2; ";
        let res = Parser::new(src).parse().unwrap();
        let v = make_if(eq_expr(), sum_stmt(), Some(sub_stmt()));
        assert_eq!(res[0], v);
    } 

    #[test]
    fn if_without_else() {
        let src = "if (1==2) 1+2;";
        let res = Parser::new(src).parse().unwrap();
        let v = make_if(eq_expr(), sum_stmt(), None);
        assert_eq!(res[0], v);
    }
    
    #[test]
    fn if_with_block() {
        let src = "if (1==2) { 1+2; 1-2; }";
        let res = Parser::new(src).parse().unwrap();
        let v = make_if(eq_expr(), make_block(vec![sum_stmt(), sub_stmt()]), None);
        assert_eq!(res[0], v);
    }

    fn and_expr() -> Expr {
        make_and(make_num_literal("1"), make_num_literal("0"))
    }
    fn and_stmt() -> Stmt { make_expr(and_expr()) }
    
    fn or_expr() -> Expr {
        make_or(make_num_literal("1"), make_num_literal("0"))
    }
    fn or_stmt() -> Stmt { make_expr(or_expr()) }

    #[test]
    fn logicals() {
        let src = "1 and 0;";
        let res = Parser::new(src).parse().unwrap();
        assert_eq!(res[0], and_stmt());
        
        let src = "1 or 0;";
        let res = Parser::new(src).parse().unwrap();
        assert_eq!(res[0], or_stmt());
    }

    #[test]
    fn nested_logical() {
        let src = "1 and 1 or 1 and 0;";
        let res = Parser::new(src).parse().unwrap();
        let v = make_or(make_and(make_num_literal("1"), make_num_literal("1")), 
                        make_and(make_num_literal("1"), make_num_literal("0")));
        let v = make_expr(v);
        assert_eq!(res[0], v);
    }

    #[test]
    fn while_statement() {
        let src = "while (1 and 0) 1+2;";
        let res = Parser::new(src).parse().unwrap();
        let v = make_while(and_expr(), sum_stmt());
        assert_eq!(res[0], v);
    }

    fn init_stmt() -> Stmt {
        make_vardecl("i", Some(make_num_literal("1")))
    }

    fn cond_expr() -> Expr {
        make_binaryop(BinaryOpType::Less, 
            make_variable("i"), 
            make_num_literal("10"))
    }
    
    fn incr_expr() -> Expr {
        make_assign("i", make_binaryop(BinaryOpType::Add, make_variable("i"), make_num_literal("1")))
    }

    fn for_stmt() -> Stmt {
        let b = make_print(make_variable("i"));
        make_for(Some(init_stmt()), Some(cond_expr()), Some(incr_expr()), b)
    }

    #[test]
    fn for_statement() {
        let src = "for (var i = 1; i < 10; i = i + 1) print i;";
        let res = Parser::new(src).parse().unwrap();
        assert_eq!(res[0], for_stmt());
    }

    #[test]
    fn for_statement_no_decl() {
        let src = "for (; i < 10; i = i + 1) print i;";
        let b = make_print(make_variable("i"));
        let v = make_for(None, Some(cond_expr()), Some(incr_expr()), b);
        let res = Parser::new(src).parse().unwrap();
        assert_eq!(res[0], v);
    }
    
    #[test]
    fn for_statement_no_decl_no_cond() {
        let src = "for (; ; i = i + 1) print i;";
        let b = make_print(make_variable("i"));
        let v = make_for(None, None, Some(incr_expr()), b);
        let res = Parser::new(src).parse().unwrap();
        assert_eq!(res[0], v);
    }

    #[test]
    fn for_statement_no_decl_no_cond_no_incr() {
        let src = "for (;;) print i;";
        let b = make_print(make_variable("i"));
        let v = make_for(None, None, None, b);
        let res = Parser::new(src).parse().unwrap();
        assert_eq!(res[0], v);
    }

    #[test]
    fn function_call() {
        let src = "foo(bar, 3*baz, 1 and 1, \"a string literal\");";
        let res = Parser::new(src).parse().unwrap();

        let v1 = make_variable("bar");
        let v2 = make_binaryop(BinaryOpType::Mult, make_num_literal("3"), make_variable("baz"));
        let v3 = make_logical(BinaryOpType::And, make_num_literal("1"), make_num_literal("1"));
        let v4 = make_str_literal("a string literal");
        let fc = make_call(make_variable("foo"), vec![v1, v2, v3, v4]);
        let fc = make_expr(fc);
        
        assert_eq!(res[0], fc);
    }

    #[test]
    fn function_call_no_args() {
        let src = "foo();";
        let res = Parser::new(src).parse().unwrap();

        let fc = make_call(make_variable("foo"), vec![]);
        let fc = make_expr(fc);
        
        assert_eq!(res[0], fc);
    }
    
    #[test]
    fn function_call_from_closure_no_args() {
        let src = "foo()()();";
        let res = Parser::new(src).parse().unwrap();

        let fc = make_call(make_variable("foo"), vec![]);
        let fc = make_call(fc, vec![]);
        let fc = make_call(fc, vec![]);
        let fc = make_expr(fc);
        
        assert_eq!(res[0], fc);
    }
    
    #[test]
    fn function_call_currying() {
        let src = "foo(x)(y)(z);";
        let res = Parser::new(src).parse().unwrap();

        let fc = make_call(make_variable("foo"), vec![make_variable("x")]);
        let fc = make_call(fc, vec![make_variable("y")]);
        let fc = make_call(fc, vec![make_variable("z")]);
        let fc = make_expr(fc);
        
        assert_eq!(res[0], fc);
    }

}
