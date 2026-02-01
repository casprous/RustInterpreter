/** * File: main.rs 
 * Author: Cole Sprouse(casprous) 
 * Date Created: 9/1/2025
 * Most Recent Update: 11/28/2025
 * Purpose: Acts as an interpeter for pre-formatted JSON input of expressions.
 * Implements a lexically-scoped language with mutable state, recursion, and static type checking (via external typecheck binary).
 */ 

use serde::Deserialize; 
use std::collections::HashMap; 
use std::io::{self, Read}; 
use std::process; 
use std::rc::Rc;
use std::cell::RefCell;

/** 
 * Enum representing the possible expression types parsed from JSON input. 
 * This matches the expected JSON structure in which the key indicates the type. 
 **/ 
#[derive(Deserialize, Debug)] 
#[derive(Clone)] 
enum Expression { 
    Identifier(String), 
    String(String), 
    Number(i64), 
    Boolean(String), 
    ArithmeticFunction(char), 
    Application(ApplicationStruct), 
    Cond(Vec<CondClause>), 
    Let(LetStruct), 
    Block(Vec<Expression>), 
    Lambda(LambdaStruct), 
    Assignment(AssignmentStruct),
    Def(DefStruct),
} 

/** 
 * Struct representing the Operator in the Application
 **/ 
#[derive(Deserialize, Debug, Clone)] 
struct Operator { 
    #[serde(rename = "Identifier")] 
    identifier: String, 
} 

/** 
 * Struct representing the Application expression
 **/ 
#[derive(Deserialize, Debug, Clone)] 
struct ApplicationStruct { 
    #[serde(rename = "Operator")] 
    operator: Operator, 
    #[serde(rename = "Operands")] 
    operands: Vec<Expression>, 
} 

/** 
 * Struct representing a clause in the Cond expression
 **/ 
#[derive(Deserialize, Debug, Clone)] 
struct CondClause { 
    #[serde(rename = "Test")] 
    test: Box<Expression>, 
    #[serde(rename = "Consequent")] 
    consequent: Box<Expression>, 
} 

/** 
 * Struct representing the Parameter in the Let expression
 **/ 
#[derive(Deserialize, Debug, Clone)] 
struct Parameter { 
    #[serde(rename = "Name")] 
    name: String, 
    // Type is parsed but ignored by the interpreter (handled by typechecker)
    // Uses serde_json::Value to accept either simple strings (":int") or complex arrays ([":int", ":int"])
    #[serde(rename = "Type")] 
    _typ: serde_json::Value, // Prefix with '_' to silence dead code warning
} 

/** 
 * Struct representing the Let expression. 
 **/ 
#[derive(Deserialize, Debug, Clone)] 
struct LetStruct { 
    #[serde(rename = "Parameter")] 
    parameter: Parameter, 
    #[serde(rename = "Expression")] 
    expression: Box<Expression>, 
    #[serde(rename = "Block")] 
    block: Vec<Expression>, 
} 

/** 
 * Struct representing the Lambda expression. 
 **/ 
#[derive(Deserialize, Debug, Clone)] 
struct LambdaStruct { 
    #[serde(rename = "Parameters")] 
    parameters: Vec<Parameter>, 
    // Type is parsed but ignored by the interpreter (handled by typechecker)
    #[serde(rename = "Type")] 
    _typ: serde_json::Value, // Prefix with '_' to silence dead code warning
    #[serde(rename = "Block")] 
    block: Vec<Expression>, 
} 

/** 
 * Struct representing the Assignment expression.
 **/
#[derive(Deserialize, Debug, Clone)]
struct AssignmentStruct {
    #[serde(rename = "Identifier")]
    identifier: String,
    #[serde(rename = "Expression")]
    expression: Box<Expression>,
}

/** 
 * Struct representing the Definition expression.
 **/
#[derive(Deserialize, Debug, Clone)]
struct DefStruct {
    #[serde(rename = "Parameter")]
    parameter: Parameter,
    #[serde(rename = "Expression")]
    expression: Box<Expression>,
}

// Type alias for the new environment structure:
// HashMap -> Rc (Shared Ownership) -> RefCell (Interior Mutability) -> Value
type Environment = HashMap<String, Rc<RefCell<Value>>>;

/** 
 * Enum representing the possible values stored in the environment. 
 * Values can be 32-bit integers, strings, characters, booleans, or closures.
 **/ 
#[derive(Clone, Debug)] 
enum Value { 
    Number(i32), 
    String(String), 
    Character(char), 
    Boolean(bool), 
    Closure { 
        params: Vec<String>, 
        body: Vec<Expression>, 
        env: Environment, 
    },
    Null,
} 

/** 
 * Function responsible for evaluating expressions 
 * Input: 
 * expr: The expression being evaluated 
 * env: The current environment in the form of a hashmap 
 * Output: 
 * The result of the expression evaluation 
 **/ 
fn eval(expr: Expression, env: &Environment) -> Result<Value, String> { 
    
    // All currently implemented expressions 
    match expr { 
        
        // If the expression is just a number to be printed 
        Expression::Number(n) => { 
            
            // Check if number fits within 32 bit integer bounds. 
            if n > i32::MAX as i64 || n < i32::MIN as i64 { 
                return Err("Number too large for signed 32-bit integer".to_string()); 
            } 
            
            // Return value as 32 bit integer 
            Ok(Value::Number(n as i32)) 
        } 
        
        // If the expression is a string, return string surrounded by double quotes 
        Expression::String(s) => Ok(Value::String(s)), 
        
        // If the expression is a boolean 
        Expression::Boolean(s) => { 
            if s == "true" { 
                Ok(Value::Boolean(true)) 
            } else if s == "false" { 
                Ok(Value::Boolean(false)) 
            } else { 
                Err(format!("Invalid boolean value: {}", s)) 
            } 
        } 
        
        // If the expression is one of the arithmetic functions(+-*/==), return as a character 
        Expression::ArithmeticFunction(c) => Ok(Value::Character(c)), 
        
        // If the expression is an identifier, attempt to look up identifier in environment 
        Expression::Identifier(id) => { 
            env.get(&id)
               .ok_or_else(|| format!("Unbound identifier: {}", id))
               .map(|cell| cell.borrow().clone()) // Borrow the cell and clone the Value inside
        } 
       
        // If the expression is an application, attempt to evaluate the contained expression 
        Expression::Application(app) => { 
            
            // Evaluate arguments first
            let args: Result<Vec<Value>, String> = app.operands 
                .iter() 
                .map(|e| eval(e.clone(), env)) 
                .collect(); 
            let args = args?; 

            // Look up the operator in the environment
            // Supports dynamic reassignment (e.g., assign add = mul)
            let func_name = &app.operator.identifier;
            let func_cell = env.get(func_name)
                .ok_or_else(|| format!("Unknown function: {}", func_name))?;
            let func_value = func_cell.borrow().clone();

            // Dispatch based on the retrieved VALUE (not the name string)
            match func_value {
                
                // Arithmetic (+, -, *, /)
                Value::Character('+') => {
                    // If the expression is "add", perform addition and return value 
                    if args.len() != 2 { return Err("add expects exactly 2 arguments".to_string()); }
                    match (&args[0], &args[1]) { 
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)), 
                        _ => Err("add expects numbers".to_string()), 
                    } 
                },
                Value::Character('-') => {
                    // If the expression is "sub", perform subtraction and return value 
                    if args.len() != 2 { return Err("sub expects exactly 2 arguments".to_string()); }
                    match (&args[0], &args[1]) { 
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)), 
                        _ => Err("sub expects numbers".to_string()), 
                    } 
                },
                Value::Character('*') => {
                    // If the expression is "mul", perform multiplication and return value 
                    if args.len() != 2 { return Err("mul expects exactly 2 arguments".to_string()); }
                    match (&args[0], &args[1]) { 
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b)), 
                        _ => Err("mul expects numbers".to_string()), 
                    } 
                },
                Value::Character('/') => {
                    // If the expression is "div", perform division and return value 
                    if args.len() != 2 { return Err("div expects exactly 2 arguments".to_string()); }
                    match (&args[0], &args[1]) { 
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a / b)), 
                        _ => Err("div expects numbers".to_string()), 
                    } 
                },
                Value::Character('%') => {
                    // If the expression is "mod", perform modulus and return value 
                    if args.len() != 2 { return Err("mod expects exactly 2 arguments".to_string()); }
                    match (&args[0], &args[1]) { 
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a % b)), 
                        _ => Err("mod expects numbers".to_string()), 
                    } 
                },
                Value::Character('^') => {
                    // If the expression is "pow", takes the first value to the power of the second 
                    if args.len() != 2 { return Err("pow expects exactly 2 arguments".to_string()); }
                    match (&args[0], &args[1]) { 
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a.pow(*b as u32))), 
                        _ => Err("pow expects numbers".to_string()), 
                    }
                },
                
                // Logic (==, >, <)
                Value::Character('=') => { // "=="
                    // If the expression is "==", test equivalence on the two values within 
                    if args.len() != 2 { return Err("== expects exactly 2 arguments".to_string()); }
                    match (&args[0], &args[1]) { 
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a == b)), 
                        (Value::String(a), Value::String(b)) => Ok(Value::Boolean(a == b)),
                        _ => Err("== expects two numbers or two strings".to_string()), 
                    } 
                },
                Value::Character('>') => {
                    // If the expression is ">", return boolean result
                    if args.len() != 2 { return Err("> expects exactly 2 arguments".to_string()); }
                    match (&args[0], &args[1]) { 
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a > b)), 
                        _ => Err("> expects numbers".to_string()), 
                    } 
                },
                Value::Character('<') => {
                    // If the expression is "<", return boolean result
                    if args.len() != 2 { return Err("< expects exactly 2 arguments".to_string()); }
                    match (&args[0], &args[1]) { 
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a < b)), 
                        _ => Err("< expects numbers".to_string()), 
                    } 
                },
                
                // Math helpers
                Value::Character('√') => { // "sqrt"
                    // If the expression is "sqrt", takes the square root of the value 
                    if args.len() != 1 { return Err("sqrt expects exactly 1 argument".to_string()); }
                    match &args[0] { 
                        Value::Number(a) => { 
                            let num = *a as f64;
                            if num < 0.0 { return Err("sqrt expects a non-negative number".to_string()); }
                            // Convert back to i32
                            Ok(Value::Number(num.sqrt() as i32)) 
                        } 
                        _ => Err("sqrt expects a number".to_string()), 
                    } 
                },
                Value::Character('∑') => { // "sum"
                    // If the expression is "sum", perform summation and return value 
                    if args.len() < 2 { return Err("sum expects at least 2 arguments".to_string()); }
                    let mut sum = 0; 
                    for arg in args { 
                        match arg { 
                            Value::Number(n) => sum += n, 
                            _ => return Err("sum expects numbers".to_string()), 
                        } 
                    } 
                    Ok(Value::Number(sum)) 
                },
                
                // String Functions (Stored as Strings in Env)
                Value::Character('p') => { // "print"
                    // If the expression is "print", prints the arguments to stdout
                    if args.is_empty() { return Err("print expects at least 1 argument".to_string()); }
                    for arg in args {
                        match arg {
                            Value::String(s) => print!("{}\n", s),
                            Value::Number(n) => print!("{}\n", n),
                            Value::Boolean(b) => print!("\'{}\'\n", b),
                            _ => return Err("print expects strings, numbers, or booleans".to_string()),
                        }
                    }
                    Ok(Value::Null) // Returns Null
                },
                Value::Character('l') => { // "len"
                    // If the expression is "len", gets the length of the string
                    if args.len() != 1 { return Err("len expects exactly 1 argument".to_string()); }
                    match &args[0] { 
                        Value::String(s) => Ok(Value::Number(s.len() as i32)), 
                        _ => Err("len expects a string".to_string()), 
                    } 
                },
                Value::String(ref s) if s == "^2" => { // "sqr"
                    // If the expression is "sqr", squares the value 
                    if args.len() != 1 { return Err("sqr expects exactly 1 argument".to_string()); }
                    match &args[0] { 
                        Value::Number(a) => Ok(Value::Number(a * a)), 
                        _ => Err("sqr expects a number".to_string()), 
                    } 
                },
                Value::String(ref s) if s == "concat" => {
                    // If the expression is "concat", concatenates string representations of arguments
                    let mut output = String::new();
                    for arg in args {
                        match arg {
                            Value::String(s) => output.push_str(&s),
                            Value::Number(n) => output.push_str(&n.to_string()),
                            Value::Boolean(b) => output.push_str(&b.to_string()),
                            _ => return Err("concat expects strings, numbers, or booleans".to_string()),
                        }
                    }
                    Ok(Value::String(output)) // Return the built string
                },
                
                Value::String(ref s) if s == "dquote" => {
                    // Returns a double quote character as a string
                    if !args.is_empty() { return Err("dquote expects 0 arguments".to_string()); }
                    Ok(Value::String("\"".to_string()))
                },

                Value::String(ref s) if s == "n√" => { // "root"
                    // If the expression is "root", takes the nth root of the first value 
                    if args.len() != 2 { return Err("root expects exactly 2 arguments".to_string()); }
                    match (&args[0], &args[1]) { 
                        (Value::Number(a), Value::Number(b)) => { 
                            let base = *a as f64; 
                            let n = *b as f64; 
                            if base < 0.0 { return Err("root expects a non-negative number".to_string()); }
                            if n == 0.0 { return Err("cannot take zeroth root".to_string()); }
                            Ok(Value::Number(base.powf(1.0 / n).round() as i32)) 
                        } 
                        _ => Err("root expects numbers".to_string()), 
                    } 
                },

                Value::String(ref s) if s == "substring" => {
                    // If the expression is "substring", gets a substring
                    if args.len() != 3 { return Err("substring expects (string, start, length)".to_string()); }
                    match (&args[0], &args[1], &args[2]) {
                        (Value::String(ref s), Value::Number(start), Value::Number(len)) => {
                            let start_usize = *start as usize;
                            let len_usize = *len as usize;
                            if start_usize > s.chars().count() { return Err("substring start index out of bounds".to_string()); }
                            let sub: String = s.chars().skip(start_usize).take(len_usize).collect();
                            Ok(Value::String(sub))
                        },
                        _ => Err("substring expects (String, Number, Number)".to_string()),
                    }
                },
                Value::String(ref s) if s == "indexOf" => {
                    // If the expression is "indexOf", finds the index of a substring
                    if args.len() != 2 { return Err("indexOf expects (string, substring)".to_string()); }
                    match (&args[0], &args[1]) {
                        (Value::String(haystack), Value::String(needle)) => {
                            match haystack.find(needle) {
                                Some(index) => Ok(Value::Number(index as i32)),
                                None => Ok(Value::Number(-1)), 
                            }
                        },
                        _ => Err("indexOf expects (String, String)".to_string()),
                    }
                },
                Value::String(ref s) if s == "stringToInt" => {
                    // If the expression is "stringToInt", parses a string into an integer
                    if args.len() != 1 { return Err("stringToInt expects 1 argument".to_string()); }
                    match &args[0] {
                        Value::String(s) => match s.parse::<i32>() {
                            Ok(num) => Ok(Value::Number(num)),
                            Err(_) => Err(format!("Could not parse '{}' as an integer", s)),
                        },
                        _ => Err("stringToInt expects a String".to_string()),
                    }
                },
                Value::String(ref s) if s == "intToString" => {
                    // If the expression is "intToString", converts an integer to a string
                    if args.len() != 1 { return Err("intToString expects 1 argument".to_string()); }
                    match &args[0] {
                        Value::Number(n) => Ok(Value::String(n.to_string())),
                        _ => Err("intToString expects a Number".to_string()),
                    }
                },
                Value::String(ref s) if s == ">=" => {
                    // If the expression is ">=", return boolean result
                    if args.len() != 2 { return Err(">= expects 2 arguments".to_string()); }
                    match (&args[0], &args[1]) { 
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a >= b)), 
                        _ => Err(">= expects numbers".to_string()), 
                    } 
                },
                Value::String(ref s) if s == "<=" => {
                    // If the expression is "<=", return boolean result
                    if args.len() != 2 { return Err("<= expects 2 arguments".to_string()); }
                    match (&args[0], &args[1]) { 
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a <= b)), 
                        _ => Err("<= expects numbers".to_string()), 
                    } 
                },

                // User Defined Closures
                Value::Closure { params, body, env: captured_env } => { 
                    if params.len() != args.len() { 
                        return Err(format!("Expected {} args, got {}", params.len(), args.len())); 
                    } 
                    
                    // Create new environment, wrapping args in new cells
                    let mut local_env = captured_env.clone(); 
                    for (p, a) in params.into_iter().zip(args.into_iter()) { 
                        local_env.insert(p, Rc::new(RefCell::new(a))); 
                    } 
                    
                    // Evaluate the function body with the new local_env
                    let mut last = None; 
                    if body.is_empty() { 
                        return Ok(Value::Null); // Return Null for empty body
                    } 
                    for e in body { 
                        last = Some(eval(e, &local_env)?); 
                    } 
                    Ok(last.unwrap()) 
                },
                
                _ => Err(format!("Type {} is not a function", func_name)) 
            } 
        } 
        
        // If the expression is a cond, evaluate the clauses in order 
        Expression::Cond(clauses) => { 
            
            // Check to make sure cond isn't empty
            if clauses.is_empty() { 
                return Err("Empty cond".to_string()); 
            } 
            
            // Iterate over clauses
            for clause in clauses { 
                
                // Evaluate clause and assign to temporary value
                let test_val = eval(*clause.test, env)?; 
                
                // Check equivalence
                if let Value::Boolean(b) = test_val { 
                    if b { 
                        return eval(*clause.consequent, env); 
                    } 
                } else { 
                    return Err("Cond test must evaluate to boolean".to_string()); 
                } 
            } 
            Err("No matching clause in cond".to_string()) 
        } 
        
        // If the expression is a let, bind the variable and evaluate the block 
        Expression::Let(let_struct) => { 
            // Evaluate the RHS in the *current* environment
            let val = eval(*let_struct.expression, env)?; 
            
            // Create new local environment
            let mut local_env = env.clone(); 
            
            // Create a *new cell* for this variable
            let cell = Rc::new(RefCell::new(val.clone()));

            // Insert the new cell into the local environment
            local_env.insert(let_struct.parameter.name.clone(), cell); 
            
            // Check if let block is empty. If it is, return the value of the binding.
            if let_struct.block.is_empty() { 
                return Ok(val); 
            } 
            
            // Track last evaluated expression
            let mut last = None; 
            
            // Iterate over all expressions in block
            for e in let_struct.block { 
                last = Some(eval(e, &local_env)?); // Use the new local_env
            } 
            
            // Return last and unwrap it
            Ok(last.unwrap()) 
        } 
        
        // If the expression is a block, evaluate the expressions in order and return the last 
        Expression::Block(exprs) => { 
            
            // Check if block is empty
            if exprs.is_empty() { 
                return Ok(Value::Null); // Return Null for empty block
            } 
            
            // Create a new environment scope for this block
            let mut block_env = env.clone();
            
            // Pass 1: Find all Defs
            // Add all defined names to the block_env, initialized to Null.
            // Allows for mutual recursion.
            for e in &exprs {
                if let Expression::Def(def_struct) = e {
                    let name = def_struct.parameter.name.clone();
                    let cell = Rc::new(RefCell::new(Value::Null));
                    block_env.insert(name, cell);
                }
            }

            // Pass 2: Evaluate all expressions
            // Use the new block_env that contains the empty cells.
            let mut last = Ok(Value::Null); // Default result for a block
            for e in exprs {
                last = eval(e, &block_env);
                if last.is_err() {
                    return last; // Propagate errors
                }
            }
            last // Return the result of the last expression
        } 

        // If the expression is a lambda, create a closure with captured environment
        Expression::Lambda(lam) => { 
            // Convert params to vector
            let params: Vec<String> = lam.parameters.iter().map(|p| p.name.clone()).collect(); 
           
            // Create and return closure, capturing the current environment
            Ok(Value::Closure { 
                params, 
                body: lam.block.clone(), 
                env: env.clone(), // This clones the Rcs, preserving shared state
            }) 
        } 

        // If the expression is an assignment
        Expression::Assignment(assign_struct) => {
            // Evaluate the RHS
            let new_value = eval(*assign_struct.expression, env)?;
            
            // Find the cell in the environment
            let cell = env.get(&assign_struct.identifier)
                .cloned()
                .ok_or_else(|| format!("Cannot assign to unbound identifier: {}", assign_struct.identifier))?;
            
            // Mutate the value *inside* the cell
            *cell.borrow_mut() = new_value;
            
            Ok(Value::Null) // Return Null
        }

        // If the expression is a definition
        Expression::Def(def_struct) => {
            // Evaluate the RHS (e.g., the lambda)
            let new_value = eval(*def_struct.expression, env)?;
            
            // Find the cell (which was created in Pass 1 of the Block eval)
            let cell = env.get(&def_struct.parameter.name)
                .cloned()
                .ok_or_else(|| format!("Compiler Error: Def'd cell not found for: {}", def_struct.parameter.name))?;
            
            // Mutate the value in the cell (from Null to the actual function)
            *cell.borrow_mut() = new_value;
            
            Ok(Value::Null) // Return Null
        }
    } 
} 

/** 
 * Main is the entry point for the interpreter. It reads JSON input from stdin, 
 * parses it into an Expression, interprets it using the environment and prints the result or an error. 
 **/ 
fn main() { 
    
    // Initialize environment as HashMap mapping identifiers to Values as such: <Ident,Val> 
    let mut env: Environment = HashMap::new(); 
    
    // Environment setup
    // Wrap initial values in Rc<RefCell<...>>
    env.insert("x".to_string(), Rc::new(RefCell::new(Value::Number(10)))); 
    env.insert("v".to_string(), Rc::new(RefCell::new(Value::Number(5)))); 
    env.insert("i".to_string(), Rc::new(RefCell::new(Value::Number(1)))); 
    env.insert("author".to_string(), Rc::new(RefCell::new(Value::String("casprous".to_string())))); 
    env.insert("COMMA".to_string(), Rc::new(RefCell::new(Value::String(",".to_string()))));
    
    // Arithmetic (Characters)
    env.insert("add".to_string(), Rc::new(RefCell::new(Value::Character('+')))); 
    env.insert("sub".to_string(), Rc::new(RefCell::new(Value::Character('-')))); 
    env.insert("mul".to_string(), Rc::new(RefCell::new(Value::Character('*')))); 
    env.insert("div".to_string(), Rc::new(RefCell::new(Value::Character('/')))); 
    env.insert("mod".to_string(), Rc::new(RefCell::new(Value::Character('%')))); 
    env.insert("pow".to_string(), Rc::new(RefCell::new(Value::Character('^')))); 
    env.insert("sum".to_string(), Rc::new(RefCell::new(Value::Character('∑')))); 
    env.insert("sqrt".to_string(), Rc::new(RefCell::new(Value::Character('√')))); 
    env.insert("print".to_string(), Rc::new(RefCell::new(Value::Character('p'))));
    env.insert("len".to_string(), Rc::new(RefCell::new(Value::Character('l'))));
    env.insert("==".to_string(), Rc::new(RefCell::new(Value::Character('=')))); 
    env.insert(">".to_string(), Rc::new(RefCell::new(Value::Character('>'))));
    env.insert("<".to_string(), Rc::new(RefCell::new(Value::Character('<'))));

    // String Helpers (String Keys)
    env.insert("sqr".to_string(), Rc::new(RefCell::new(Value::String("^2".to_string())))); 
    env.insert("root".to_string(), Rc::new(RefCell::new(Value::String("n√".to_string())))); 
    env.insert(">=".to_string(), Rc::new(RefCell::new(Value::String(">=".to_string())))); 
    env.insert("<=".to_string(), Rc::new(RefCell::new(Value::String("<=".to_string()))));
    env.insert("substring".to_string(), Rc::new(RefCell::new(Value::String("substring".to_string()))));  
    env.insert("stringToInt".to_string(), Rc::new(RefCell::new(Value::String("stringToInt".to_string())))); 
    env.insert("intToString".to_string(), Rc::new(RefCell::new(Value::String("intToString".to_string())))); 
    env.insert("indexOf".to_string(), Rc::new(RefCell::new(Value::String("indexOf".to_string()))));
    env.insert("concat".to_string(), Rc::new(RefCell::new(Value::String("concat".to_string()))));
    env.insert("dquote".to_string(), Rc::new(RefCell::new(Value::String("dquote".to_string()))));
    
    // Read entire input from stdin into string
    let mut input = String::new(); 
    if let Err(e) = io::stdin().read_to_string(&mut input) { 
        eprintln!("Error reading input: {}", e); 
        process::exit(1); 
    } 
    
    // Parse input string as JSON into Expression enum
    let expression: Expression = match serde_json::from_str(input.trim()) { 
        Ok(e) => e, 
        Err(e) => { 
            eprintln!("Error parsing JSON: {}", e); 
            process::exit(1); 
        } 
    }; 
    
    // Evaluate expression
    match eval(expression, &env) { 
        // All currently implemented valid Values 
        Ok(Value::Number(n)) => println!("{}", n), 
        Ok(Value::String(s)) => println!("{}", s), 
        Ok(Value::Character(c)) => println!("\'{}\'", c), 
        Ok(Value::Boolean(b)) => println!("\'{}\'", b), 
        Ok(Value::Closure { .. }) => println!("<closure>"), 
        Ok(Value::Null) => (), // Suppress output for Null
        Err(e) => { 
            eprintln!("Error: {}", e); 
            process::exit(1); 
        } 
    } 
}