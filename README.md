## Some design decisions I took

1. (---) is implemented as a dashed line ONLY when these are the only 3 characters on any line. This was done for more readability and so any random --- does not expand to a horizontal rule. also, no whitespace characters should be present before or after. (after this only the "\n" character must be there)

2. Nested lists are a little weird as done by John Gruber. Its unclear how many spaces are needed for each nest. In markdown one space would be enough to make a nested list (1st degree), but if you add 2 or 3 spaces before next element it wouldn't nest another list and make a 2nd degree list but instead be considered as part of the same 1st degree list. example is the 3rd point, of which you can see the source of.
3. hello
 * 1space
  * 2space
   * 3space
    * 4space
     * 5space

4. Codeblocks are implemented as according to markdown, so if an indented block follows a paragraph without a blank line in between, it will be considered to be a part of the paragraph instead of a codeblock. Hence a blankline separating codeblock from paragraphs are necessary