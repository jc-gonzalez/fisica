#!/usr/local/bin/perl -w
#######################################################################
#
# SuS
#
# Program to generated a commented, LaTeX version of 
# a source code.
#
# Copyright (C) 1998 J C Gonzalez
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
#----------------------------------------------------------------------
#
#  Created: Thu May  7 16:24:22 1998
#  Author:  Jose Carlos Gonzalez
#  Purpose: Perl program to get documentation out of comments
#           in source codes
#  Notes:   Generates a commented, LaTeX version of the code
#    
#----------------------------------------------------------------------
# $RCSfile$
# $Revision$
# $Author$ 
# $Date$
#######################################################################
#
# Notes:  Documentation still to come...
#
#######################################################################

#use strict;
#use vars qw($VERSION $SYSDIR);
#use Carp;

require 5.003;

$PROGRAM = 'SUS';
$VERSION = '0.1';

$PROGRAMTEXDEF = '\def\SuS{{\it SuS\/}}%';
$PROGRAMTEX = '\SuS{}';

$latex = 1;
$html = 0; 

%sus_actions=( 
               '@author' => \&act_set_author,
               '@title' => \&act_set_title,
               '@function' => \&act_dofunc,
               '@email' => \&act_email,
               '@desc' => \&act_set_desc,
               '@code' => \&act_codeon,
               '@endcode' => \&act_codeoff,
               '@comment' => \&act_comment,
               '@file' => \&act_set_file,
               '@maintitle' => \&act_write_maintitle,
               '@header' => \&act_write_header,
               '@pagestitle' => \&act_set_pages_title,
               '@text' => \&act_text,
               '@name' => \&act_name,
               '@rulers' => \&act_rulers,
               '@section' => \&act_sec,
               '@subsection' => \&act_subsec,
               '@subsubsection' => \&act_subsubsec,
               '@bye' => \&act_bye,
               '@T' => \&act_TeX,
               '@tableofcontents' => \&act_tableofcontents,
               '@listoffigures' => \&act_listoffigures,
               '@listoftables' => \&act_listoftables,
               '@fancyheadings' => \&act_fancyheadings,
               '@getcodeonly' => \&act_getcodeonly,
               );

$sus_file="samplefile";
$sus_title="Sample File"; 
$sus_description="Sample File generated by $PROGRAM version $VERSION";
$sus_author="$PROGRAM version $VERSION";
$sus_pages_title=$sus_title;
$sus_code=0;
$sus_skip=0;
$sus_name='';
$sus_date=localtime;
$isfancyheadings=0;
$tableofcontents=0;
$listoffigures=0;
$listoftables=0;
$rulers=0;
$sus_email='';

###
# subroutines
###

# presentation
sub hello {
    print STDERR <<"_eom_";
============================================================
$PROGRAM version $VERSION
Copyright (c) J C Gonzalez, 1998
All rights reserved
============================================================

_eom_
}

# get options for the program
sub get_options {
    foreach $op ( @ARGV ) { 
        shift;
        last if ($op =~ /^--$/);
        if ($op =~ /^-L/) { $latex = 1; $html = 0; }
        if ($op =~ /^-H/) { $latex = 0; $html = 1; }
    }
    1;
}

###
#  main procedure
###

# say hello
hello;

# get command line options
#get_options;

while ($line = <>) {
    
    while ( ($key,$action) = each %sus_actions ) {
        if ( $line =~ /$key/ ) {
            &$action($line);
        }
    }
    
    if ( $sus_code == 1 ) {
        if ( $sus_skip == 0 ) {
            print "$line";
        } else {
            $sus_skip = 0;
        }
    }
    
}

&act_bye;


###
#  subroutines
###

sub parse {
    my ($key,$line)=@_;
    @sline=split ' ',$line;
    for ( $i=0; $i < $#sline; $i++ ) {
        last if ("$sline[$i]" =~ $key);
    }
    $sline[$i+1];
}

sub parseline {
    my ($key,$line)=@_;
    if ( "$line" =~ /(.*)$key (.*)/ ) {
        $2;
    }
}

sub act_set_title {
    $sus_title = parseline('@title',@_);
}

sub act_set_file {
    $sus_file = parse('@file',@_);
}

sub act_set_pages_title {
    $sus_pages_title = parseline('@pagestitle',@_);
}

sub act_set_author {
    $sus_author = parseline('@author',@_);
}

sub act_TeX {
    print parseline('@T',@_),"\n";
}

sub act_sec {
    print "\\section\{".parseline('@section',@_)."\}\n\n";
}

sub act_subsec {
    print "\\subsection\{".parseline('@subsection',@_)."\}\n\n";
}

sub act_subsubsec {
    print "\\subsubsection\{".parseline('@subsubsection',@_)."\}\n\n";
}

sub act_set_desc {
    $sus_description = parseline('@desc',@_);
}

sub act_name {
    $sus_name = parseline('@name',@_);
    $sus_name =~ s/_/\\_/g;
}

sub act_date {
    $sus_date = parseline('@date',@_);
}

sub act_email {
    $sus_email = parseline('@email',@_);
}

sub act_dofunc {
    print << "eom";
\\subsubsection\{$sus_name\}
\\begin\{tabular\}\{ll\}
\\hline
Name:&\\sl $sus_name\\\\
Author:&\\it $sus_author\\/\\\\
Date:&\\it $sus_date\\/\\\\
Description:&\\it $sus_description\\/\\\\
\\hline
\\end\{tabular\}
\\\\
eom
}

sub act_text {
    my ($l,@texto);
    print parseline('@text',@_),"\n";
    while ($l = <>) {
        last if ($l =~ /\@endtext/);
        if ( $l =~ s/(\/\/|[\#\*]+)\s//g ) {
            print $l . " ";
        } else {
            print $l . " ";
        }
    }
}

sub act_comment {
    my ($l);

    # identical to @code
    print "\n\\end{verbatim}\n";
#    print "\\hrule width \\textwidth height 1pt\n" if ( $rulers == 1 );
    print "\\normalsize\n\n";

    #identical to @text
    print parseline('@comment',@_),"\n";
    while ($l = <>) {
        last if ($l =~ /\@endcomment/);
        if ( $l =~ s/(\/\/|[\#\*]+)\s//g ) {
            print $l . " ";
        } else {
            print $l . " ";
        }
    }

    # identical to @endcode
    print "\n\\small\n";
#    print "\\hrule width \\textwidth height 1pt\n" if ( $rulers == 1 );
    print "\\begin{verbatim}\n";
    $sus_skip=1;
}

sub act_rulers {
    if ( $rulers == 0 ) {
        $rulers = 1;
    } else {
        $rulers = 0;
    }
}

sub act_codeon {
    $sus_code=1;
    print "\n\\small\n";
#    print "\\hrule width \\textwidth height 1pt\n" if ( $rulers == 1 );
    print "\\begin{verbatim}\n";
    $sus_skip=1;
}

sub act_codeoff {
    $sus_code=0;
    print "\n\\end{verbatim}\n";
#    print "\\hrule width \\textwidth height 1pt\n" if ( $rulers == 1 );
    print "\\normalsize\n\n";
}

sub act_write_maintitle {
    my (@l)=@_;
    print << "eom1";
\\documentclass[10pt,a4paper]{article}
\\usepackage[german,english]{babel}
\\usepackage{fancyheadings}
\\usepackage{portland}
\\usepackage{supertab}
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

sub act_write_header {
    my (@l)=@_;
    print << "eom";

\\begin{center}
{\\Large \\bfseries
$sus_title\\\\
$sus_author\\\\}
\\end{center}

eom
}

sub act_bye {
    print "\\rm\n";
    print "\\newpage\\tableofcontents\n" if ( $tableofcontents == 1);
    print "\\newpage\\listoffigures\n" if ( $listoffigures == 1);
    print "\\newpage\\listoftables\n" if ( $listoftables == 1);
    print "\n\\end{document}\n";
    exit 0;
}

sub act_tableofcontents {
    $tableofcontents=1;
}

sub act_listoffigures {
    $listoffigures=1;
}

sub act_listoftables {
    $listoftables=1;
}

sub act_fancyheadings {
    $isfancyheadings = 1;
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
