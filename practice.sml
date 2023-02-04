include IMPERATIVE_IO

exception AsterixNotMatched;


fun HeadString(cnt,0) = "<h"^(String.str(Char.chr(cnt + Char.ord(#"0")))^">")
    |   HeadString(cnt,a) = "</h"^(String.str(Char.chr(cnt + Char.ord(#"0")))^">");

HeadString(6,1)

(*the first argument in Parse denotes the current character number, and 2 denotes if we are in a paragraph, 3 denotes if we are in bold, 4 denotes if we are in italics*)

fun mdt2html(infile) =
    let
        val ins = TextIO.openIn infile;
        val outs = TextIO.openOut "filename.html";

        (* fun bold(#"*" :: #"*" :: t) *)

        fun Links(#"<"::s,toDisplay) = (TextIO.output(outs,"<a href=\""); Links(s,toDisplay)) (*required links to have http at the front*)
        |   Links(#">"::s,toDisplay) = (TextIO.output(outs, toDisplay^"\">"^toDisplay^"</a>"); s)
        |   Links(h::s,toDisplay) = Links(s,(toDisplay^String.str(h)));

        fun escape(#"." :: t ) = (TextIO.output1(outs,#"."); t) (*chr(92) is \ (backslash) and hence is used to escape characters*)
        |   escape(#"\\" ::  t ) = (TextIO.output1(outs,#"\\"); t) (*if \ is followed by a non-escapable character then it is printed normally*)
        |   escape(#"*" :: t ) = (TextIO.output1(outs,#"*"); t)
        |   escape( #"_" :: t ) = (TextIO.output1(outs,#"_"); t)
        |   escape( #"{" :: t ) = (TextIO.output1(outs,#"{"); t)
        |   escape( #"}" :: t ) = (TextIO.output1(outs,#"}"); t)
        |   escape( #"#" :: t ) = (TextIO.output1(outs,#"#"); t)
        |   escape( #"[" :: t ) = (TextIO.output1(outs,#"["); t)
        |   escape( #"(" :: t ) = (TextIO.output1(outs,#"("); t)
        |   escape( #")" :: t ) = (TextIO.output1(outs,#")"); t)
        |   escape( #"]" :: t ) = (TextIO.output1(outs,#"]"); t)
        |   escape( #"-" :: t ) = (TextIO.output1(outs,#"-"); t)
        |   escape( #"+" :: t ) = (TextIO.output1(outs,#"+"); t)
        |   escape( #"!" :: t ) = (TextIO.output1(outs,#"!"); t)
        |   escape( #"`" :: t ) = (TextIO.output1(outs,#"`"); t)
        |   escape(t) = t; (*printed normally if inescapable character*)

        fun Parse([],a,b,c,d) = (b,c,d) (*empty so we should just return if we are in a paragraph*)
        |   Parse(h::t,0,0,c,d) = (TextIO.output(outs,"<p>\n"); Parse(h::t,0,1,c,d))
        |   Parse(#"\\" :: t,a,b,c,d) = Parse(escape(t),a,b,c,d)
        |   Parse(#"<" :: #"h" :: #"t":: #"t" :: #"p":: t,a,b,c,d) = Parse(Links(#"<" :: #"h" :: #"t" :: #"t" :: #"p":: t,""),a,b,c,d)
        |   Parse(#"*" :: #"*" :: t,a,b,0,d) = (TextIO.output(outs,"<strong>"); Parse(t,a,b,1,d))
        |   Parse(#"*" :: #"*" :: t,a,b,1,d) = (TextIO.output(outs,"</strong>"); Parse(t,a,b,0,d))
        |   Parse(#"*" :: t,a,b,c,0) = (TextIO.output(outs,"<em>"); Parse(t,a,b,c,1))
        |   Parse(#"*" :: t,a,b,c,1) = (TextIO.output(outs,"</em>"); Parse(t,a,b,c,0))
        |   Parse(h::t,a,b,c,d) = (TextIO.output1(outs,h); Parse(t,a+1,1,c,d));

        fun header(#"#"::t,cnt,1,c,d) = (TextIO.output(outs,"</p>\n"); header(#"#"::t,cnt,0,c,d)) 
        |   header(#"#"::t,cnt,0,c,d) = if (cnt < 5) then header(t,cnt+1,0,c,d)
        else (TextIO.output(outs,HeadString(6,0)); Parse(t,6,0,c,d); TextIO.output(outs,HeadString(6,1)); (0,c,d))
        |   header(s,0,a,c,d) = Parse(s,0,a,c,d)
        |   header(s,cnt,a,c,d) = (TextIO.output(outs,HeadString(cnt,0)); Parse(s,cnt,0,c,d); TextIO.output(outs,HeadString(cnt,1)); (0,c,d))


        (*fun LineToList(Line)*)

        fun LineWork(NONE,0,0,0) = (TextIO.closeIn ins; TextIO.closeOut outs)
        |   LineWork(NONE,0,c,d) = (TextIO.output(outs, "Asterix wasnt matched"); TextIO.closeIn ins; TextIO.closeOut outs; raise AsterixNotMatched)(*raise AsterixNotMatched*)
        |   LineWork(NONE,b,c,d) = (TextIO.output(outs,"</p>\n"); TextIO.closeIn ins; TextIO.closeOut outs)
        |   LineWork(SOME("\n"),1,c,d) = (TextIO.output(outs,"</p>\n"); LineWork(TextIO.inputLine ins, 0,c,d))
        |   LineWork(SOME("---\n"),b,c,d) = (TextIO.output(outs,"<hr />\n"); LineWork(TextIO.inputLine ins, b,c,d))
        |   LineWork(SOME(line),b,c,d) = 
            let
                val (isInPara, isBold, isItalic) = header(explode line,0,b,c,d);
            in
            LineWork(TextIO.inputLine ins, isInPara, isBold, isItalic)
            end; 
    in
        LineWork(TextIO.inputLine ins, 0,0,0)
    end;





mdt2html "MarkdownTest.md";
mdt2html "ExampleFile.md";

