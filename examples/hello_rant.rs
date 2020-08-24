use rant::Rant;

fn main() -> Result<(), Box<dyn std::error::Error>> {
  // Create a default Rant context and load the standard library
  let mut rant = Rant::new();

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