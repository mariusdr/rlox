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
    fn vardecl_without_init() {
        let src = "var x;";
        let res = Parser::new(src).parse().unwrap();
        let vd = make_vardecl("x", None);
        assert_eq!(res[0], vd);
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


}