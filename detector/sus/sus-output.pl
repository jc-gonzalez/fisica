##------------------------------------------------------------

sub Lsection {
    print "\\section\{".$1."\}\n\n";
}

sub Lsubsection {
    print "\\subsection\{".$1."\}\n\n";
}

sub Lsubsubsection {
    print "\\subsubsection\{".$1."\}\n\n";
}


sub Lfunction {
    my($v);
    print << "eom";
\\begin\{samepage\}
\\subsubsection\{$sus_name\}
\\framebox[14cm][l]\{
\\begin\{tabular\}\{ll\}
%%\\hline
Name:&\\bf $sus_name\\\\
Author:&\\it $sus_author\\/\\\\
Date:&\\it $sus_date\\/\\\\
Description:&\\it $sus_description\\/\\\\
Variables:&\\\\
eom
    foreach $variable (sort keys %var) {
        $vv = $var{$variable}[0];
        $vv =~ tr/@/\*/;
        print "\\hfill\{\\tt \\bf ",
        $vv,"\}:&",
        $var{$variable}[1],"\\\\\n";
    }

    print << "eom";
Returns:&\\it $sus_freturn\\/\\\\
%%\\hline
\\end\{tabular\}
\}
\\end\{samepage\}
\\\\

eom
    %var=();
    $nvar=0;
    $sus_freturn='nothing';
}

sub Lcomment {
    my ($l);

    # (almost) identical to @code
    print "\n\\end{verbatim}\n";
    print "\\normalsize\n\n";

    # (almost) identical to @text
    print parseline('@comment',@_),"\n";
    while ($l = <>) {
        last if ($l =~ /\@endcomment/);
        if ( $l =~ s/(\/\/|[\#\*]+)\s//g ) {
            print $l . " ";
        } else {
            print $l . " ";
        }
    }

    # (almost) identical to @endcode
    print "\n\\small\n";
    print "\\begin{verbatim}\n";
    $sus_skip=1;
}

sub Lcodeon {
    print "\n\\small\n";
    print "\\begin{verbatim}\n";
}

sub Lcodeoff {
    print "\n\\end{verbatim}\n";
    print "\\normalsize\n\n";
}

sub Lmaintitle {
    my (@l)=@_;
    print << "eom1";
\\documentclass[10pt,a4paper]{report}
\\usepackage[german,english]{babel}
\\usepackage{fancyheadings}
\\usepackage{graphicx}
\\usepackage{moreverb}
\\usepackage{float}
\\usepackage{amsmath}
\\usepackage{amssymb}

\\input{entries.tex}
\\floatstyle{ruled}
\\newfloat{Table}{thp}{loT}
\\setlength{\\labelwidth}{40pt}%
\\setlength{\\hoffset}{-1cm}
\\setlength{\\textwidth}{15cm}

\\pagestyle{fancy}

$PROGRAMTEXDEF
eom1

    if ($isfancyheadings == 1) {
    &fancyheadings;
    }

    print << "eom2";

\\begin{document}

\\thispagestyle{empty}
\\mbox{}
\\vskip 5cm
\\parbox[c]{\\textwidth}{
{\\huge \\bfseries
$sus_title
\\\\}
\\hrule width \\textwidth height 3pt
\\flushright{$sus_description}\\par
\\vskip 0.5cm
\\vskip 7cm
\\flushleft{\\large
$sus_author\\\\
Last update: $sus_date\\\\
\\hrule width \\textwidth height 2pt
}
}
 
\\newpage
\\thispagestyle{empty}
\\mbox{}
\\input{sus.disc}\\vfill
\\mbox{}\\\\
Copyright \$\\copyright\$ \\number\\year, $sus_author (\{\\tt $sus_email\})\\\\
Last update: $sus_date\\\\
Printed in: \\today\\\\
\\\\
{\\it Generated using $PROGRAMTEX version $VERSION \\\/}
\\newpage

eom2

}

sub Lwriteheader {
    my (@l)=@_;
    print << "eom";

\\begin{center}
{\\Large \\bfseries
$sus_title\\\\
$sus_author\\\\}
\\end{center}

eom
}

sub Lbye {
    print "\\rm\n";
    print "\\newpage\\tableofcontents\n" if ( $tableofcontents == 1);
    print "\\newpage\\listoffigures\n" if ( $listoffigures == 1);
    print "\\newpage\\listoftables\n" if ( $listoftables == 1);
    print "\n\\end{document}\n";
    exit 0;
}

##------------------------------------------------------------

sub Hsection {
    print "<H1>".$1."</H1>\n\n";
}

sub Hsubsection {
    print "<H2>".$1."</H2>\n\n";
}

sub Hsubsubsection {
    print "<H3>".$1."</H3>\n\n";
}


sub Hfunction {
    my($v);
    print << "eom";
<HR>
<H4>$sus_name </H4>
<DT>
<DH>$sus_name
<DD>
<DH>Name:<DD><strong>$sus_name</strong>
<DH>Author:<DD><em>$sus_author</em>
<DH>Date:<DD><em>$sus_date</em>
<DH>Description:<DD><em>$sus_description</em>
<DH>Variables:<DD>
    <DT COMPACT>
eom
    foreach $variable (sort keys %var) {
        $vv = $var{$variable}[0];
        $vv =~ tr/@/\*/;
        print "<dh>" . $vv . "\n";
        print "<dd>" . $var{$variable}[1],"\n";
    }
    </DT>
    print << "eom";
<DH>Returns:<DD><em>$sus_freturn</em>
</DT>

eom
    %var=();
    $nvar=0;
    $sus_freturn='nothing';
}

sub Hcomment {
    my ($l);

    # (almost) identical to @code
    print "</code>\n";

    print "\n<blockquote>\n";

    # (almost) identical to @text
    print parseline('@comment',@_),"\n";
    while ($l = <>) {
        last if ($l =~ /\@endcomment/);
        if ( $l =~ s/(\/\/|[\#\*]+)\s//g ) {
            print $l . " ";
        } else {
            print $l . " ";
        }
    }

    # (almost) identical to @endcode
    print "\n</blockquote>\n";
    print "<code>\n";
    $sus_skip=1;
}

sub Hcodeon {
    print "\n<code>\n";
}

sub Hcodeoff {
    print "\n</code>\n";
}

sub Hmaintitle {
    my (@l)=@_;
    print << "eom1";
<HTML>
<HEAD>
<TITLE>$PROGRAMTEXDEF</TITLE>
</HEAD>
<BODY>
eom1

    print << "eom2";

<BR><BR><HR>
<STRONG>$sus_title</STRONG><BR>
<EM>$sus_description</EM>
<HR>
$sus_author<BR>
Last update: $sus_date<BR>
<HR>
<FONT SIZE=-1>
Copyright &copy; 1999, $sus_author <keyb>$sus_email</keyb><hr>
Generated using $PROGRAMTEX version $VERSION<hr>

eom2

}

sub Hwriteheader {
    my (@l)=@_;
    print << "eom";

<P ALIGN=CENTER>
<STRONG>$sus_title</STRONG><HR>
<EM>$sus_author</EM>
</P>

eom
}

sub Hbye {
    print << "eom3";
</BODY>
</HTML>
    exit 0;
}



## this routine is still not working

sub fancyheadings {
    print << "eom";

\\pagestyle{fancyplain}
\\makeatletter
\\addtolength{\\headwidth}{60pt}
\\renewcommand{\\sectionmark}[1]%
        {\\markboth{\\thesection\\ \#1}{}}
\\renewcommand{\\subsectionmark}[1]%
        {\\markright{\\thesubsection\\ \#1}}
\\rhead{}
\\lhead{}
\\chead[%
    \\if\@mainmatter \\fancyplain{}{\\small\\scshape\\leftmark} 
    \\else \\fancyplain{}{\\sl\\headname}
    \\fi]{%
    \\if\@mainmatter \\fancyplain{}{\\small\\slshape\\rightmark} 
    \\else \\fancyplain{}{\\sl\\headname}
    \\fi}
\\rfoot{}
\\lfoot{}
\\cfoot{%
    \\if\@mainmatter \\sl\\arabic{page} \\ \\else \\sl\\roman{page} \\ \\fi}
\\makeatother

eom
}



#EOF
