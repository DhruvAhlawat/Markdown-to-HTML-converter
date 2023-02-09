
fun printRepeat file cnt = 
let
    val outs = TextIO.openOut "InputMarkdown.md"; 
    fun recOut(0) = ()
    |   recOut(cnt) = (TextIO.output(outs," - list item \n"); recOut(cnt-1));
in 
    (recOut(cnt); TextIO.closeOut outs)
end;


printRepeat "InputMarkdown.md" 20


