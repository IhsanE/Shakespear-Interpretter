#1 Split the file into 3 lists, dramatis personae, settings, and the dialogue.
  - Get dramatis list
  - Get settings list
  - Get dialogue list

#2 Store the above in respective variables

#3 Make a function that parses dramatis into a list of bindings. (Variable name list)

#4 Define a function that parses settings into a list of bindings. (Function name list)

#5 Define a function that parses a single line of dialogue into its value.

#6 Recursively iterate over each line of dialogue and make a list out of their values.

#7 Done!

EVALUATING SECTION

#1 Replace all personal references with the <speakers name>

#2 Replace the ‘song of <name> and <param>’ with the output when it’s passed to function-parser.

#2.5 Evaluate #2 using #3.

#3 Look for arithmetic expression (there will only be one)

   yes

	#4 Evaluate the left/right sides
	
		#4.5 If word is in Dramatis Personae, replace with value

	#5 Compute arithmetic expression
   no

	#6 Evaluate the whole thing

		#7 Call #4.5

#8 Return a list of the above values




(
(“name” “sentence”)
()
)