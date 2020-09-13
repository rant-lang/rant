use rant::Rant;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
  // Create a Rant context and load the standard library
  let mut rant = Rant::with_random_seed();

  // Compile a simple program
  let program = rant.compile_quiet(r#"
  [$greet:name] {
    {Hello|Hi|Hey} <name>!
  }
  [greet:world]
  "#)?;

  // Run the program and fetch the result string
  let output = rant.run(&program)?;
  println!("{}", output);
  Ok(())
}