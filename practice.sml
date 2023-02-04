include IMPERATIVE_IO

exception AsterixNotMatched;


fun HeadString(cnt,0) = "<h"^(String.str(Char.chr(cnt + Char.ord(#"0")))^">")
    |   HeadString(cnt,a) = "</h"^(String.str(Char.chr(cnt + Char.ord(#"0")))^">");

HeadString(6,1)

(*the first argument in Parse denotes the current character number, and 2 denotes if we are in a paragraph, 
3 denotes if we are in bold, 4 denotes if we are in italics*)
val ins = TextIO.openIn "MarkdownTest.md";
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

        fun Parse([],a,b,c,d,e) = (b,c,d,e) (*empty so we should just return if we are in a paragraph*)
        |   Parse(h::t,0,0,c,d,e) = (TextIO.output(outs,"<p>\n"); Parse(h::t,0,1,c,d,e))
        |   Parse(#"\\" :: t,a,b,c,d,e) = Parse(escape(t),a,b,c,d,e)
        |   Parse(#"<" :: #"h" :: #"t":: #"t" :: #"p":: t,a,b,c,d,e) = Parse(Links(#"<" :: #"h" :: #"t" :: #"t" :: #"p":: t,""),a,b,c,d,e)
        |   Parse(#"*" :: #"*" :: t,a,b,0,d,e) = (TextIO.output(outs,"<strong>"); Parse(t,a,b,1,d,e))
        |   Parse(#"*" :: #"*" :: t,a,b,1,d,e) = (TextIO.output(outs,"</strong>"); Parse(t,a,b,0,d,e))
        |   Parse(#"*" :: t,a,b,c,0,e) = (TextIO.output(outs,"<em>"); Parse(t,a,b,c,1,e))
        |   Parse(#"*" :: t,a,b,c,1,e) = (TextIO.output(outs,"</em>"); Parse(t,a,b,c,0,e))
        |   Parse(#"_" :: t,a,b,c,d,0) = (TextIO.output(outs,"<u>"); Parse(t,a,b,c,d,1))
        |   Parse(#" " :: t,a,b,c,d,1) = (TextIO.output(outs,"</u>"); Parse(t,a,b,c,d,0))
        |   Parse(h::t,a,b,c,d,e) = (TextIO.output1(outs,h); Parse(t,a+1,1,c,d,e));

        fun header(#"#"::t,cnt,1,c,d,e) = (TextIO.output(outs,"</p>\n"); header(#"#"::t,cnt,0,c,d,e)) 
        |   header(#"#"::t,cnt,0,c,d,e) = if (cnt < 5) then header(t,cnt+1,0,c,d,e)
        else (TextIO.output(outs,HeadString(6,0)); Parse(t,6,0,c,d,e); TextIO.output(outs,HeadString(6,1)); (0,c,d,e))
        |   header(s,0,a,c,d,e) = Parse(s,0,a,c,d,e)
        |   header(s,cnt,a,c,d,e) = (TextIO.output(outs,HeadString(cnt,0)); Parse(s,cnt,0,c,d,e); TextIO.output(outs,HeadString(cnt,1)); (0,c,d,e))


        

        fun LineWork(NONE,0,0,0,0) = (TextIO.closeIn ins; TextIO.closeOut outs)
        |   LineWork(NONE,b,0,0,e) = (TextIO.output(outs,"</p>\n"); TextIO.closeIn ins; TextIO.closeOut outs) (*closes the open paragraph*)
        |   LineWork(NONE,b,c,d,e) = (TextIO.output(outs, "Asterix wasnt matched"); TextIO.closeIn ins; TextIO.closeOut outs; raise AsterixNotMatched)(*raise AsterixNotMatched*)
        |   LineWork(SOME("\n"),1,c,d,e) = (TextIO.output(outs,"</p>\n"); LineWork(TextIO.inputLine ins, 0,c,d,e))
        |   LineWork(SOME("---\n"),b,c,d,e) = (TextIO.output(outs,"<hr />\n"); LineWork(TextIO.inputLine ins, b,c,d,e))
        |   LineWork(SOME(line),b,c,d,e) = 
            let
                val (isInPara, isBold, isItalic, isUnderlined) = header(explode line,0,b,c,d,e);
            in
            LineWork(TextIO.inputLine ins, isInPara, isBold, isItalic, isUnderlined)
            end; 
    in
        LineWork(TextIO.inputLine ins, 0,0,0,0)
    end;





mdt2html "MarkdownTest.md";
mdt2html "ExampleFile.md";

