#![feature(box_syntax)]

extern crate asexp;
extern crate expression;
extern crate closed01;

use expression::{ExpressionError, Expression};
use asexp::Sexp;
use closed01::Closed01;

#[derive(Clone, PartialEq, Debug)]
pub enum Closed01Expr {
    /// References a variable by position
    Var(usize),

    /// A constant value.
    Const(Closed01<f32>),

    Min(Box<Closed01Expr>, Box<Closed01Expr>),
    Max(Box<Closed01Expr>, Box<Closed01Expr>),
    Distance(Box<Closed01Expr>, Box<Closed01Expr>),
    Avg(Box<Closed01Expr>, Box<Closed01Expr>),
    SatAdd(Box<Closed01Expr>, Box<Closed01Expr>),
    SatSub(Box<Closed01Expr>, Box<Closed01Expr>),
    Mul(Box<Closed01Expr>, Box<Closed01Expr>),
    ScaleUp(Box<Closed01Expr>, Box<Closed01Expr>),
    ScaleDown(Box<Closed01Expr>, Box<Closed01Expr>),
    Inv(Box<Closed01Expr>),
    Round(Box<Closed01Expr>),
}

impl Expression for Closed01Expr {
    type Element = Closed01<f32>;

    fn evaluate(&self, variables: &[Self::Element]) -> Result<Self::Element, ExpressionError> {
        Ok(match self {
            &Closed01Expr::Var(n) => {
                try!(variables.get(n).ok_or(ExpressionError::InvalidVariable)).clone()
            }
            &Closed01Expr::Const(val) => val,

            &Closed01Expr::Min(ref e1, ref e2) => {
                try!(e1.evaluate(variables)).min(try!(e2.evaluate(variables)))
            }
            &Closed01Expr::Max(ref e1, ref e2) => {
                try!(e1.evaluate(variables)).max(try!(e2.evaluate(variables)))
            }
            &Closed01Expr::Distance(ref e1, ref e2) => {
                try!(e1.evaluate(variables)).distance(try!(e2.evaluate(variables)))
            }
            &Closed01Expr::Avg(ref e1, ref e2) => {
                Closed01::avg(try!(e1.evaluate(variables)), try!(e2.evaluate(variables)))
            }
            &Closed01Expr::SatAdd(ref e1, ref e2) => {
                try!(e1.evaluate(variables)).saturating_add(try!(e2.evaluate(variables)))
            }
            &Closed01Expr::SatSub(ref e1, ref e2) => {
                try!(e1.evaluate(variables)).saturating_sub(try!(e2.evaluate(variables)))
            }
            &Closed01Expr::Mul(ref e1, ref e2) => {
                try!(e1.evaluate(variables)).mul(try!(e2.evaluate(variables)))
            }
            &Closed01Expr::ScaleUp(ref e1, ref e2) => {
                try!(e1.evaluate(variables)).scale_up(try!(e2.evaluate(variables)))
            }
            &Closed01Expr::ScaleDown(ref e1, ref e2) => {
                try!(e1.evaluate(variables)).scale_down(try!(e2.evaluate(variables)))
            }

            &Closed01Expr::Inv(ref e1) => {
                try!(e1.evaluate(variables)).inv()
            }

            &Closed01Expr::Round(ref e1) => {
                if try!(e1.evaluate(variables)) < Closed01::middle() {
                    Closed01::zero()
                } else {
                    Closed01::one()
                }
            }
        })
    }
}

impl<'a> Into<Sexp> for &'a Closed01Expr {
    fn into(self) -> Sexp {
        match self {
            &Closed01Expr::Var(n) => Sexp::from(format!("${}", n)),
            &Closed01Expr::Const(n) => n.get().into(),
            &Closed01Expr::Min(ref a, ref b) => {
                Sexp::from(("min",
                            Into::<Sexp>::into(a.as_ref()),
                            Into::<Sexp>::into(b.as_ref())))
            }
            &Closed01Expr::Max(ref a, ref b) => {
                Sexp::from(("max",
                            Into::<Sexp>::into(a.as_ref()),
                            Into::<Sexp>::into(b.as_ref())))
            }
            &Closed01Expr::Distance(ref a, ref b) => {
                Sexp::from(("distance",
                            Into::<Sexp>::into(a.as_ref()),
                            Into::<Sexp>::into(b.as_ref())))
            }
            &Closed01Expr::Avg(ref a, ref b) => {
                Sexp::from(("avg",
                            Into::<Sexp>::into(a.as_ref()),
                            Into::<Sexp>::into(b.as_ref())))
            }
            &Closed01Expr::SatAdd(ref a, ref b) => {
                Sexp::from(("sat+",
                            Into::<Sexp>::into(a.as_ref()),
                            Into::<Sexp>::into(b.as_ref())))
            }
            &Closed01Expr::SatSub(ref a, ref b) => {
                Sexp::from(("sat-",
                            Into::<Sexp>::into(a.as_ref()),
                            Into::<Sexp>::into(b.as_ref())))
            }
            &Closed01Expr::Mul(ref a, ref b) => {
                Sexp::from(("*",
                            Into::<Sexp>::into(a.as_ref()),
                            Into::<Sexp>::into(b.as_ref())))
            }
            &Closed01Expr::ScaleUp(ref a, ref b) => {
                Sexp::from(("scale+",
                            Into::<Sexp>::into(a.as_ref()),
                            Into::<Sexp>::into(b.as_ref())))
            }
            &Closed01Expr::ScaleDown(ref a, ref b) => {
                Sexp::from(("scale-",
                            Into::<Sexp>::into(a.as_ref()),
                            Into::<Sexp>::into(b.as_ref())))
            }
            &Closed01Expr::Inv(ref a) => {
                Sexp::from(("inv",
                            Into::<Sexp>::into(a.as_ref())))
            }
            &Closed01Expr::Round(ref a) => {
                Sexp::from(("round",
                            Into::<Sexp>::into(a.as_ref())))
            }
        }
    }
}

#[test]
fn test_expr() {
    let expr =
        Closed01Expr::Min(box Closed01Expr::Var(0),
                          box Closed01Expr::Const(Closed01::new(0.4)));

    assert_eq!(Ok(Closed01::new(0.4)), expr.evaluate(&[Closed01::new(0.4)]));
    assert_eq!(Ok(Closed01::new(0.4)), expr.evaluate(&[Closed01::new(0.9)]));
    assert_eq!(Ok(Closed01::new(0.1)), expr.evaluate(&[Closed01::new(0.1)]));
}
