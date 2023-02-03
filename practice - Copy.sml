include IMPERATIVE_IO

fun HeadString(cnt,0) = "<h"^(String.str(Char.chr(cnt + Char.ord(#"0")))^">")
    |   HeadString(cnt,a) = "</h"^(String.str(Char.chr(cnt + Char.ord(#"0")))^">");

HeadString(6,1)

(*the first argument in Parse denotes the current character number, and 2 denotes if we are in a paragraph, 3 denotes if we are in bold, 4 denotes if we are in italics*)

fun mdt2html(infile) =
    let
        val ins = TextIO.openIn infile;
        val outs = TextIO.openOut "filename.html";

        fun bold(#"*" :: #"*" :: t)

        fun Links(#"<"::s,toDisplay) = (TextIO.output(outs,"<a href=\""); Links(s,toDisplay))
        |   Links(#">"::s,toDisplay) = (TextIO.output(outs, toDisplay^"\">"^toDisplay^"</a>"); s)
        |   Links(h::s,toDisplay) = Links(s,(toDisplay^String.str(h)));

        fun Parse([],a,b,c) = (b,c) (*empty so we should just return if we are in a paragraph*)
        |   Parse(#"<" :: #"h" :: #"t":: #"t" :: #"p":: t,a,b,c) = Parse(Links(#"<" :: #"h" :: #"t" :: #"t" :: #"p":: t,""),a,b,c)
        |   Parse(#"*" :: #"*" :: t,a,b,c) = 
        |   Parse(h::t,0,0,c) = (TextIO.output(outs,"<p>\n"); Parse(h::t,0,1,c))
        |   Parse(h::t,a,b,c) = (TextIO.output1(outs,h); Parse(t,a+1,1,c));

        fun header(#"#"::t,cnt,1,c) = (TextIO.output(outs,"</p>\n"); header(#"#"::t,cnt,0,c)) 
        |   header(#"#"::t,cnt,0,c) = if (cnt < 5) then header(t,cnt+1,0,c)
        else (TextIO.output(outs,HeadString(6,0)); Parse(t,6,0); TextIO.output(outs,HeadString(6,1)); (0,c))
        |   header(s,0,a) = Parse(s,0,a,c)
        |   header(s,cnt,a) = (TextIO.output(outs,HeadString(cnt,0)); Parse(s,cnt,0); TextIO.output(outs,HeadString(cnt,1)); 0)


        (*fun LineToList(Line)*)

        fun LineWork(NONE,0) = (TextIO.closeIn ins; TextIO.closeOut outs)
        |   LineWork(NONE,a) = (TextIO.output(outs,"</p>\n"); TextIO.closeIn ins; TextIO.closeOut outs)
        |   LineWork(SOME("\n"),1) = (TextIO.output(outs,"</p>\n"); LineWork(TextIO.inputLine ins, 0))
        |   LineWork(SOME("---\n"),a) = (TextIO.output(outs,"<hr />\n"); LineWork(TextIO.inputLine ins, a))
        |   LineWork(SOME(line),para) = 
            let
                val isInPara = header(explode line,0,para);
            in
            LineWork(TextIO.inputLine ins, isInPara)
            end; 
    in
        LineWork(TextIO.inputLine ins, 0)
    end;

            
fun tuplestuff(a) = (a,a)

mdt2html "MarkdownTest.md";
mdt2html "ExampleFile.md";

