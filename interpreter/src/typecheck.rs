/** * File: typecheck.rs
 * Author: Cole Sprouse(casprous)
 * Date Created: 11/28/2025
 * Purpose: Performs static type checking on the JSON AST before execution.
 * Ensures type safety for assignments, function calls, and bindings.
 * Implements "Contextual Typing" to infer types when the parser output is incomplete.
 */

use serde::Deserialize;
use std::collections::HashMap;
use std::io::{self, Read};
use std::process;

// Data Structures

/** 
 * Enum representing the supported types in the language.
 * Based on the Checkpoint 5 requirements: :int, :bool, :str, :unit, and Arrow types.
 * VarArgs is a special internal type to handle 'print', 'concat', and 'sum'.
 */
#[derive(Debug, Clone, PartialEq)]
enum Type {
    Int,
    Bool,
    Str,
    Unit,
    Arrow(Vec<Type>, Box<Type>), // (Parameter Types) -> Return Type
    VarArgs, // Loophole type: matches any list of arguments
}

// Helper to convert type to string for user-friendly error messages
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, ":int"),
            Type::Bool => write!(f, ":bool"),
            Type::Str => write!(f, ":str"),
            Type::Unit => write!(f, ":unit"),
            Type::VarArgs => write!(f, "*"),
            Type::Arrow(params, ret) => {
                write!(f, "(")?;
                for (i, p) in params.iter().enumerate() {
                    if i > 0 { write!(f, " ")?; }
                    write!(f, "{}", p)?;
                }
                write!(f, " -> {})", ret)
            }
        }
    }
}

// AST Definitions (Mirrors main.rs but specialized for Type Parsing)

#[derive(Deserialize, Debug, Clone)]
#[allow(dead_code)]
enum Expression {
    Identifier(String),
    String(String),
    Number(i64),
    Boolean(String),
    Application(ApplicationStruct),
    Cond(Vec<CondClause>),
    Let(LetStruct),
    Block(Vec<Expression>),
    Lambda(LambdaStruct),
    Assignment(AssignmentStruct),
    Def(DefStruct),
}

#[derive(Deserialize, Debug, Clone)]
struct ApplicationStruct {
    #[serde(rename = "Operator")]
    operator: Box<Expression>, 
    #[serde(rename = "Operands")]
    operands: Vec<Expression>,
}

#[derive(Deserialize, Debug, Clone)]
struct CondClause {
    #[serde(rename = "Test")]
    test: Box<Expression>,
    #[serde(rename = "Consequent")]
    consequent: Box<Expression>,
}

#[derive(Deserialize, Debug, Clone)]
struct Parameter {
    #[serde(rename = "Name")]
    name: String,
    // Parses 'Type' as generic JSON Value to handle both Strings (":int") and Arrays ([":int", ":int"])
    #[serde(rename = "Type")]
    typ: serde_json::Value, 
}

#[derive(Deserialize, Debug, Clone)]
struct LetStruct {
    #[serde(rename = "Parameter")]
    parameter: Parameter,
    #[serde(rename = "Expression")]
    expression: Box<Expression>,
    #[serde(rename = "Block")]
    block: Vec<Expression>,
}

#[derive(Deserialize, Debug, Clone)]
struct LambdaStruct {
    #[serde(rename = "Parameters")]
    parameters: Vec<Parameter>,
    #[serde(rename = "Type")]
    typ: serde_json::Value, // Return type
    #[serde(rename = "Block")]
    block: Vec<Expression>,
}

#[derive(Deserialize, Debug, Clone)]
struct AssignmentStruct {
    #[serde(rename = "Identifier")]
    identifier: String,
    #[serde(rename = "Expression")]
    expression: Box<Expression>,
}

#[derive(Deserialize, Debug, Clone)]
struct DefStruct {
    #[serde(rename = "Parameter")]
    parameter: Parameter,
    #[serde(rename = "Expression")]
    expression: Box<Expression>,
}

/** 
 * Parses a JSON value into our internal Type enum.
 * Handles base types (":int") and complex arrow types represented as arrays.
 * Returns Ok(None) for empty strings to handle C parser bugs gracefully.
 */
fn parse_type(v: &serde_json::Value) -> Result<Option<Type>, String> {
    match v {
        serde_json::Value::String(s) => match s.as_str() {
            ":int" => Ok(Some(Type::Int)),
            ":bool" => Ok(Some(Type::Bool)),
            ":str" => Ok(Some(Type::Str)),
            ":unit" => Ok(Some(Type::Unit)),
            "" => Ok(None), // Workaround for when C Parser sometimes outputs "" for complex types
            ":untyped" => Err("Type Error: Code contains ':untyped'. Annotations required.".to_string()),
            _ => Err(format!("Type Error: Unknown base type '{}'", s)),
        },
        serde_json::Value::Array(arr) => {
            // Arrow type format: [arg1, arg2, ..., return_type]
            if arr.is_empty() {
                return Err("Type Error: Found empty type array []".to_string());
            }
            // The last element is the return type
            let return_type = parse_type(arr.last().unwrap())?
                .ok_or_else(|| "Type Error: Nested arrow return type missing".to_string())?;

            // The preceding elements are parameter types
            let mut param_types = Vec::new();
            for i in 0..arr.len() - 1 {
                let pt = parse_type(&arr[i])?
                    .ok_or_else(|| "Type Error: Nested arrow param type missing".to_string())?;
                param_types.push(pt);
            }

            Ok(Some(Type::Arrow(param_types, Box::new(return_type))))
        }
        _ => Err(format!("Type Error: Invalid JSON structure for type: {:?}", v)),
    }
}

/** * Checks if two types are compatible.
 * Strict equality for base types.
 * Structural equality for Arrow types.
 * VarArgs is universally compatible (the "Loophole").
 */
fn compatible_types(t1: &Type, t2: &Type) -> bool {
    match (t1, t2) {
        (Type::Int, Type::Int) => true,
        (Type::Bool, Type::Bool) => true,
        (Type::Str, Type::Str) => true,
        (Type::Unit, Type::Unit) => true,
        (Type::VarArgs, _) => true, 
        (_, Type::VarArgs) => true, 
        (Type::Arrow(p1, r1), Type::Arrow(p2, r2)) => {
            // Check return types first
            if !compatible_types(r1, r2) { return false; }
            // Check arity (number of parameters)
            if p1.len() != p2.len() { return false; }
            // Recursively check all parameter types
            for (pt1, pt2) in p1.iter().zip(p2.iter()) {
                if !compatible_types(pt1, pt2) { return false; }
            }
            true
        }
        _ => false,
    }
}

/** 
 * Main recursive type checking function.
 * expr: The AST node to check.
 * env: The environment mapping identifiers to Types.
 * expected: Contextual hint for the expected type (used to recover from parser bugs).
 * Returns: The Type of the expression or an Error string.
 */
fn type_of(expr: &Expression, env: &HashMap<String, Type>, expected: Option<&Type>) -> Result<Type, String> {
    match expr {
        // Base literals
        Expression::Number(_) => Ok(Type::Int),
        Expression::String(_) => Ok(Type::Str),
        Expression::Boolean(_) => Ok(Type::Bool),
        
        // Variable lookup
        Expression::Identifier(id) => {
            env.get(id).cloned().ok_or_else(|| format!("Type Error: Undefined identifier '{}'", id))
        }

        // Function Application
        Expression::Application(app) => {
            let rator_type = type_of(&app.operator, env, None)?;
            
            match rator_type {
                Type::Arrow(param_types, return_type) => {
                    // Check arity
                    if app.operands.len() != param_types.len() {
                        return Err(format!("Type Error: Arity mismatch. Expected {} args, got {}", param_types.len(), app.operands.len()));
                    }
                    // Check arguments against parameter types
                    for (arg_expr, expected_type) in app.operands.iter().zip(param_types.iter()) {
                        let arg_type = type_of(arg_expr, env, Some(expected_type))?;
                        if !compatible_types(&arg_type, expected_type) {
                            return Err(format!("Type Error: Argument mismatch. Expected {}, got {}", expected_type, arg_type));
                        }
                    }
                    Ok(*return_type)
                }
                Type::VarArgs => {
                    // Loophole for built-ins like 'print' and 'concat'
                    for arg in &app.operands {
                        type_of(arg, env, None)?; 
                    }
                    // Special casing for variadic functions that return values
                    if let Expression::Identifier(name) = &*app.operator {
                        match name.as_str() {
                            "concat" | "intToString" | "substring" => return Ok(Type::Str),
                            "sum" => return Ok(Type::Int), 
                            _ => {}
                        }
                    }
                    Ok(Type::Unit)
                }
                _ => Err(format!("Type Error: Operator is not a function. Got {}", rator_type)),
            }
        }

        // Conditional (if-then-else)
        Expression::Cond(clauses) => {
            if clauses.is_empty() { return Ok(Type::Unit); }
            let mut first_consequent_type: Option<Type> = None;
            for clause in clauses {
                let test_type = type_of(&clause.test, env, Some(&Type::Bool))?;
                if !compatible_types(&test_type, &Type::Bool) {
                    return Err(format!("Type Error: Cond test must be :bool, got {}", test_type));
                }
                let cons_type = type_of(&clause.consequent, env, expected)?;
                // Ensure all branches return compatible types
                match &first_consequent_type {
                    None => first_consequent_type = Some(cons_type),
                    Some(first) => {
                        if !compatible_types(first, &cons_type) {
                            return Err(format!("Type Error: Incompatible Cond branches. Expected {}, got {}", first, cons_type));
                        }
                    }
                }
            }
            Ok(first_consequent_type.unwrap_or(Type::Unit))
        }

        // Let Binding
        Expression::Let(let_struct) => {
            let declared_type = parse_type(&let_struct.parameter.typ)?
                .ok_or_else(|| "Type Error: Let binding missing type annotation".to_string())?;
            
            // Pass declared type as expectation to RHS
            let rhs_type = type_of(&let_struct.expression, env, Some(&declared_type))?;

            if !compatible_types(&declared_type, &rhs_type) {
                return Err(format!("Type Error: Let binding '{}' type mismatch. Declared {}, got {}", 
                    let_struct.parameter.name, declared_type, rhs_type));
            }
            // Extend environment and check block
            let mut new_env = env.clone();
            new_env.insert(let_struct.parameter.name.clone(), declared_type);
            if let_struct.block.is_empty() {
                Ok(rhs_type)
            } else {
                type_of_block(&let_struct.block, &new_env)
            }
        }

        Expression::Block(exprs) => type_of_block(exprs, env),

        // Function Definition (Lambda)
        Expression::Lambda(lam) => {
            // Attempt to parse return type from AST
            let parsed_return = parse_type(&lam.typ)?;
            
            // Contextual Typing: If AST is broken (""), use 'expected' type from context
            let return_type = match parsed_return {
                Some(t) => t,
                None => {
                    if let Some(Type::Arrow(_, ctx_return)) = expected {
                        *ctx_return.clone()
                    } else {
                        return Err("Type Error: Parser failed to provide return type (empty string) and no context available.".to_string());
                    }
                }
            };

            let mut param_types = Vec::new();
            let mut new_env = env.clone();
            
            // Bind parameters in new environment
            for p in &lam.parameters {
                let pt = parse_type(&p.typ)?
                    .ok_or_else(|| format!("Type Error: Parameter '{}' missing type", p.name))?;
                param_types.push(pt.clone());
                new_env.insert(p.name.clone(), pt);
            }
            
            // Check function body against return type
            let body_type = type_of_block(&lam.block, &new_env)?;
            if !compatible_types(&return_type, &body_type) {
                return Err(format!("Type Error: Function body type mismatch. Declared return {}, got {}", return_type, body_type));
            }
            Ok(Type::Arrow(param_types, Box::new(return_type)))
        }

        // Variable Assignment
        Expression::Assignment(assign) => {
            let var_type = env.get(&assign.identifier)
                .ok_or_else(|| format!("Type Error: Cannot assign to unbound variable '{}'", assign.identifier))?;
            
            let rhs_type = type_of(&assign.expression, env, Some(var_type))?;
            
            if !compatible_types(var_type, &rhs_type) {
                return Err(format!("Type Error: Assignment type mismatch for '{}'. Expected {}, got {}", 
                    assign.identifier, var_type, rhs_type));
            }
            Ok(Type::Unit)
        }

        // Recursive Definition (Def)
        Expression::Def(def) => {
            // Def names are added to the environment in Pass 1 of Block check.
            // Here in Pass 2, we only check that the RHS matches the declared type.
            let declared_type = parse_type(&def.parameter.typ)?
                .ok_or_else(|| format!("Type Error: Def '{}' missing type annotation", def.parameter.name))?;
            
            let rhs_type = type_of(&def.expression, env, Some(&declared_type))?; 
            
            if !compatible_types(&declared_type, &rhs_type) {
                return Err(format!("Type Error: Def '{}' type mismatch. Declared {}, got {}", 
                    def.parameter.name, declared_type, rhs_type));
            }
            Ok(Type::Unit)
        }
    }
}

/** 
 * Helper to handle Block scoping logic (Two-Pass for Defs)
 */
fn type_of_block(exprs: &[Expression], env: &HashMap<String, Type>) -> Result<Type, String> {
    if exprs.is_empty() { return Ok(Type::Unit); }
    
    let mut block_env = env.clone();
    
    // Pass 1: Scan for Defs and extend environment (supporting recursion)
    for expr in exprs {
        if let Expression::Def(def) = expr {
            let declared_type = parse_type(&def.parameter.typ)?
                .ok_or_else(|| format!("Type Error: Def '{}' missing type", def.parameter.name))?;
            block_env.insert(def.parameter.name.clone(), declared_type);
        }
    }

    // Pass 2: Check all expressions in order
    let mut last_type = Type::Unit;
    for expr in exprs {
        last_type = type_of(expr, &block_env, None)?;
    }
    Ok(last_type)
}

/** 
 * Main Entry Point
 * Initializes the type environment with built-in functions, parses the JSON input,
 * runs the type checker, and exits with 0 on success, 1 on failure.
 */
fn main() {
    let mut env = HashMap::new();
    let math_op = Type::Arrow(vec![Type::Int, Type::Int], Box::new(Type::Int));
    
    // Initial Bindings
    env.insert("i".to_string(), Type::Int);
    env.insert("v".to_string(), Type::Int);
    env.insert("x".to_string(), Type::Int);
    env.insert("author".to_string(), Type::Str);
    env.insert("COMMA".to_string(), Type::Str);
    
    // Arithmetic
    env.insert("add".to_string(), math_op.clone());
    env.insert("sub".to_string(), math_op.clone());
    env.insert("mul".to_string(), math_op.clone());
    env.insert("div".to_string(), math_op.clone());
    env.insert("mod".to_string(), math_op.clone()); 
    env.insert("pow".to_string(), math_op.clone());
    env.insert("root".to_string(), math_op.clone());
    
    env.insert("sqr".to_string(), Type::Arrow(vec![Type::Int], Box::new(Type::Int)));
    env.insert("sqrt".to_string(), Type::Arrow(vec![Type::Int], Box::new(Type::Int)));
    
    // Comparisons
    let cmp_op = Type::Arrow(vec![Type::Int, Type::Int], Box::new(Type::Bool));
    env.insert("==".to_string(), Type::Arrow(vec![Type::VarArgs, Type::VarArgs], Box::new(Type::Bool))); // Loose equality
    env.insert(">".to_string(), cmp_op.clone());
    env.insert("<".to_string(), cmp_op.clone());
    env.insert(">=".to_string(), cmp_op.clone());
    env.insert("<=".to_string(), cmp_op.clone());

    // Variadic functions
    env.insert("print".to_string(), Type::VarArgs);
    env.insert("concat".to_string(), Type::VarArgs);
    env.insert("sum".to_string(), Type::VarArgs); 
    
    // String helpers
    env.insert("len".to_string(), Type::Arrow(vec![Type::Str], Box::new(Type::Int)));
    env.insert("substring".to_string(), Type::Arrow(vec![Type::Str, Type::Int, Type::Int], Box::new(Type::Str)));
    env.insert("indexOf".to_string(), Type::Arrow(vec![Type::Str, Type::Str], Box::new(Type::Int)));
    env.insert("stringToInt".to_string(), Type::Arrow(vec![Type::Str], Box::new(Type::Int)));
    env.insert("intToString".to_string(), Type::Arrow(vec![Type::Int], Box::new(Type::Str)));
    env.insert("dquote".to_string(), Type::Arrow(vec![], Box::new(Type::Str)));

    // Read Input
    let mut input = String::new();
    if let Err(e) = io::stdin().read_to_string(&mut input) {
        eprintln!("Error reading input: {}", e);
        process::exit(1);
    }

    // Parse JSON
    let expression: Expression = match serde_json::from_str(input.trim()) {
        Ok(e) => e,
        Err(e) => {
            eprintln!("Error parsing JSON: {}", e);
            process::exit(1);
        }
    };

    // Run Type Check
    match type_of(&expression, &env, None) {
        Ok(_) => process::exit(0),
        Err(e) => {
            eprintln!("{}", e);
            process::exit(1);
        }
    }
}