## Some design decisions I took

1. (---) is implemented as a dashed line ONLY when these are the only 3 characters on any line. This was done for more readability and so any random --- does not expand to a horizontal rule. also, no whitespace characters should be present before or after. (after this only the "\n" character must be there)

4. Codeblocks are implemented as according to markdown, so if an indented block follows a paragraph without a blank line in between, it will be considered to be a part of the paragraph instead of a codeblock. Hence a blankline separating codeblock from paragraphs are necessary

5. All escape characters are properly dealt with.

6. \* and \- need to be escaped. If any * is found that does not correspond to a bold, italic or list form then an AsterixNotMatched Error is raised and also printed in the html file at the end.

7. codeblocks works fine for four spaces always BUT they should be 4 or more spaces or 1 or more tabs.In the assignment's md file, the link "cse.iitd.ac.in" has unnatural non-printable spaces behind it, so it doesn't directly work but works after backspacing and adding normal spaces back again.

6. underlining is delimited by space. An underscore after the underlined text is not necessary and a simple space can signify the end of the underlining, but an underscore before the text is necessary. Hence  \_under\_line will also output as <u>under line</u> (a space is there as well, since thats how its given in the Assignment.). Its important to note that either newline can only delimit it iff theres an ending underscore as well, so ending with "\_\n" works fine but without the underscore it would instead go to the newline

2. Nested lists are a little weird as done by John Gruber. Its unclear how many spaces are needed for each nest. In markdown one space would be enough to make a nested list (1st degree), but if you add 2 or 3 spaces before next element it wouldn't nest another list and make a 2nd degree list but instead be considered as part of the same 1st degree list. It takes 5 spaces for a 2nd degree nested list to form in markdown. example is given as the next list, of which you can see the source of. But the way I have implemented is as follows
 -

3. example Markdown List to show the unclear number of leading spaces it takes for nesting multiple levels
 * 1space
  * 2space
   * 3space
    * 4space
     * 5space

