% Mar. 10, 2005 -- M. Stark modified
%     Switched report to book, changed evensidemargin to allow double
%     sided printing and alternation of page nubers in upper right/left
%     which is more convinient for printing a "book"
%   NOTE: do NOT use this to submit final dissertation, it does NOT
%     conform to thesis guidelines.  It is good for producing copies
%     in "book" format for your own personal use.
% psuthesis.cls -- 1996/11/10 -- S. Simpson -- simpson@math.psu.edu
% This is an experimental hack!  Use at your own risk!
% Basically this is psuthesis.sty 3.0, hacked to work with LaTeX 2e.

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{psuthesis2side}[1996/11/10]
%\DeclareOption*{\PassOptionsToClass{\CurrentOption}{report}}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{book}}
\ProcessOptions
%\LoadClass{report}
\LoadClass{book}

%\setcounter{errorcontextlines}{999}

% psuthesis.sty, revision 3.0
%
% For Pennsylvania State University doctoral theses.  This document style is
% per the Office of Theses and Graduate School Publications' "Thesis Guide"
% dated July 1990.
%
% Documentation follows the summary of revisions.
%
% Two sample files are available: psuthesissample.tex and
% psuthesissamplechapter.tex.
%
% An improved version of the BibTeX plain style file is available.  It is
% named psuthesis.bst.
%
% psuthesis.sty --- A LaTeX style file for PSU doctoral theses.
% Copyright (C) 1993 Thomas P. Kelliher
%
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation; either version 1, or (at your option)
% any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program; if not, write to the Free Software
% Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
%
% Thomas P. Kelliher
% Department of Computer Science
% 333 Whitmore Laboratory
% The Pennsylvania State University
% University Park, PA 16802
%
% kelliher@cs.psu.edu
%
% This work is derived from the psuthesis of Mirjana Obradovic, which
% itself was derived from a publicly available version of psuthesis which
% was made available by Bruce Parker.  I thank Mirjana and Bruce for their
% contributions to this work.  Also, I thank Sanjeev Dharap for his
% contributions and help.  His insight and knowledge of TeX and LaTeX have
% gone a long way toward making this style file more user friendly.
%
% If you make improvements, please bring them to my attention.
%
%
%
% psuthesis.sty,v
% Revision 3.0  1993/03/11  22:25:04  kelliher
% Next public release.  The file is now a full document style, not merely
% an option.  Both 10 and 11 point are now supported.  Additional page
% layout changes have been made by request of the Thesis Office.  A
% pseudocode environment had been added.  It is now possible to exclude the
% list of tables or list of figures.  Comments have been improved.
%
% Revision 2.1  1993/03/07  18:32:21  kelliher
% Some 11pt support added.  Other new stuff in, not commented yet.
%
% Revision 2.0  1992/05/26  21:02:17  kelliher
% Next public release.
%
% Revision 1.5  1992/05/20  14:04:08  kelliher
% Added \setscriptfonts for flexibility in scriptfonts.  Fixed some spelling
% problems.  Moved setting of \markright of thesis draft mode out of
% \frontmatter.
%
% Revision 1.4  1992/05/19  14:48:13  kelliher
% \titlepage fixed to adjust to number of lines in the title so that the
% title page is always full.
% Fixed minor problem with centering of word "Appendix" when there is only a
% single appendix.
% \head modified to take an argument.
% \pagestyle called directly from style file, rather than from within
% \endfrontmatter (where it can't take effect globally).
%
% Revision 1.3  1992/05/19  14:37:12  kelliher
% Finalized method of setting page breaking penalties and provided an
% environment for relaxing these penalties.
% Created a frontmatter environment.
% Changed defaults for where figures and tables are placed.
% Added support for appendices and the vita page.
%
% Revision 1.2  1992/05/14  00:33:09  kelliher
% Subsubsections are now numbered and included in the table of contents by
% default, at the request of the Thesis Office.
% Fixed pagebreaking penalties to prevent widow and club lines.
% Removed some deadwood code.
%
% Revision 1.1  1992/05/05  21:26:14  kelliher
% Added capability of using bold math mode in chapter and section titles.
% Unfortunately, numbers are not handled properly in bold math mode.
%
% Revision 1.0  1992/05/05  14:34:32  kelliher
% Initial revision
%
%
%
% To be used with LaTeX.  This document style may be used with the
% 11pt option.  "psuthesis" should be specified as the documentstyle.
%
% For example: \documentstyle[11pt,psfig]{psuthesis}
%
% In some cases LaTeX will have to be run three times for the page numbers
% in the table of contents to be correct.
%
% The Thesis Office prefers 11pt type (LaTeX 10pt is actually smaller than
% true 10pt).  To save paper before "officially" submitting (for the
% defense, etc.), you can decrease the line spacing by using the
% \setstretch macro.  This should be done in the preamble.
%
% For example:    \setstretch{1}
%              or \setstretch{1.3}
%
% Actually, for singlespacing it is better to enclose the entire thesis
% within a singlespace environment:
%
% \begin{singlespace}
% <all your text>
% \end{singlespace}
%
% Many doctoral students in the department were using the public version of
% psuthesis as a baseline for the private version which was used for the
% actual thesis.  In starting my thesis, I did the same, noticing that
% several facets of the style conflicted with the specifications of the
% "Thesis Guide."  I have attempted to make corrections to bring psuthesis in
% line with the "Thesis Guide."  It is my hope that other students here will
% be able to use this style without needing to make their own versions of it.
% 
% The "Thesis Guide" does permit some variance in page layout to suit the
% author's preferences.  My personal bias will be evident here.
%
% I use BibTeX to produce my bibliographies.  I don't know if not using
% BIBTeX will cause any strange problems.
%
% Remaining Work: 
% Nothing at the moment.



% New environments:
%
% frontmatter
% relaxpagebreaking
% lemma
% corollary
% theorem
% pseudocode
%
% Documentation for these new environments follows.



% Several new commands are defined, and some old ones redefined.  The new
% commands are:
%
% \setscriptfonts
%
% \blankpage
% \datethis
% \thesisdraft
% \useboldmath
% \vitalsecdepth
% \tablecaption
%
% \author
% \title
% \copyrightyear
% \submitdate
% \dept
% \readerone
% \readertwo
% \readerthree
% \readerfour
% \readerfive
% \readersix
% \readerseven
%
% \adviser
% \coadviser
% \chair
% \cochair
% \head
% \prof
% \assocprof
% \assistprof
%
% \beginfrontmatter
% \titlepage
% \signaturepage
% \committeepage
% \abstract
% \tables
% \nolotables
% \nolofigures
% \preface
% \acknowledgments
% \endfrontmatter
%
% \singleappendix
% \appendices
% \vita
%
% Documentation for these new commands and the redefined commands and
% parameters follow.



% REDEFINED COMMANDS AND PARAMETERS
%
% Several page layout parameters are changed and other commands have been
% changed according to the requirements and recommendations of the Thesis
% Office.  Changes have been made to the following:
%
% \@normalsize
% \oddsidemargin
% \evensidemargin
% \textwidth
% \marginparwidth
% \marginparsep
% \parindent
% \topmargin
% \headsep
% \textheight
% \lineskiplimit
% \lineskip
% \brokenpenalty         *
% \widowpenalty          *
% \clubpenalty           *
% \displaywidowpenalty   *
% \predisplaypenalty     *
% \postdisplaypenalty    *
% \fps@figure            *
% \fps@table             *
% \textfraction
% \floatpagefraction
% \floatsep
% \textfloatsep
% \intextsep
% \@maxsep
% \@fpsep
% \scriptsize
% \scriptscriptsize
% \footnotesize
% \tiny
% \large
% \Large
% \LARGE
% \huge
% \Huge
% \@chapnum              (Actually, this is new, but very low-level.)
% \@chapter              *
% \@makechapterhead
% \@makeschapterhead
% \l@chapter             *
% \l@section             *
% \l@subsection          *
% \l@subsubsection       *
% \l@paragraph           *
% \l@subparagraph        *
% \@pnumwidth
% \@tocrmarg
% \caption               *
% \@makecaption
% \fnum@figure
% \@begintheorem         *
% \@opargbegintheorem
% \addcontentsline
% \chapter               *
% \section
% \subsection
% \subsubsection
% \paragraph
% \subparagraph
% \tableofcontents
% \listoffigures
% \listoftables
% \bibliography          *
% \thebibliography
% \@xfloat
% \@footnotetext
% \singlespace
% \setstretch            *
% \ps@empty
% \scriptfont0           *
% \scriptfont1           *
% \scriptfont2           *
% \scriptfont3           *
% \scriptscriptfont0     *
% \scriptscriptfont1     *
% \scriptscriptfont2     *
% \scriptscriptfont3     *
% \fontdimen13\ninesy    *
% \fontdimen14\ninesy    *
% \fontdimen15\ninesy    *
% \fontdimen16\ninesy    *
% \fontdimen17\ninesy    *
% \fontdimen13\tensy     *
% \fontdimen14\tensy     *
% \fontdimen15\tensy     *
% \fontdimen16\tensy     *
% \fontdimen17\tensy     *
% \fontdimen13\elvsy     *
% \fontdimen14\elvsy     *
% \fontdimen15\elvsy     *
% \fontdimen16\elvsy     *
% \fontdimen17\elvsy     *
%
% The exact changes should be irrelevant to the "casual" user, so they
% aren't documented here.  If you need to know, dig into the depths of this
% file.  The commands marked with a "*" above will be described here,
% because the new definitions incorporate additional features which should
% be useful, or whose definitions potentially have a drastic effect on the
% layout of pages.
%
% \brokenpenalty through \postdisplaypenalty --- These page breaking
% penalties are set to their maximum values (10000) to prevent bad page
% breaks from occurring.  Situations can arise, due to these values of the
% penalties, in which truly horrible page breaks result.  The
% relaxpagebreaking environment is provided to allow you to set your own
% page breaks in such cases.
%
% \fps@figure and \fps@table are changed to place the figure or table in
% the locations given by "htbp."
%
% \caption --- Actually, \@makecaption is changed.  Captions conform to the
% Chicago Manual of Style (CMS).  \caption should only be used for figure
% captions (Use \tablecaption for tables.).  The caption should appear
% BELOW the figure.
%
% \chapter --- Again, actually \@makechapterhead is changed for what is
% described here.  Modifications have been made so as to allow for
% multiple-line chapter titles.  Each individual line, except for the final
% line, is terminated by "\\".  Automatic line breaking is TURNED OFF here,
% because lines are to be broken only according to grammatical sense.  If a
% line is too wide to fit on the page, a warning is printed.  Examples:
% \chapter{Optimal Recursive Widgets}
% \chapter{The Nocturnal Feeding Habits of \\ Sub-Saharan Spotted Sphinxes}
%
% \@chapter --- Modified so as to put the word "Chapter," "Appendix," or
% whatever in the table of contents.
%
% \l@chapter through \l@subparagraph --- Indentation for the table of
% contents entry has been increased to match the change to \@chapter.
%
% \@begintheorem --- Modified to agree with CMS.  The initial label is set
% in small caps.
%
% \bibliography --- This is changed so that the proper header phrase is
% printed and the bibliography is included in the table of contents.
%
% \setstretch --- Because this modifies \lineskiplimit and \lineskip, only
% arguments in the range [1.0,3.0) are accepted.
%
% \scriptfontn, \scriptscriptfontn --- These are modified so that all sub
% and super scripts use no smaller than nine point fonts.  This is the
% smallest point size acceptable to the Thesis Office.
%
% \fontdimen13\ninesy through \fontdimen17\elvsy --- Modified to change the
% distance that sub-/super-scripts are lowered/raised.



% NEW ENVIRONMENTS
%
% frontmatter --- The frontmatter environment is now the preferred way of
% producing frontmatter.  \beginfrontmatter remains only for the sake of
% compatibility.  A discussion of front matter follows later.
%
% relaxpagebreaking --- This environment relaxes the severe page breaking
% penalties which are set by this style in order to prevent widow lines,
% etc.  There is no way to mechanically produce perfect page breaks.  This
% environment should surround that text in which you need to set your own
% page breaks.  For example:
% \begin{relaxpagebreaking}
% <text with YOUR page breaks>
% \end{relaxpagebreaking}
%
% lemma, corollary, and theorem --- These pre-defined environments are
% useful for stating lemmas, corollaries, and theorems.  For example:
% \begin{lemma}
% All widgets are not blue-green.
% \end{lemma}
%
% pseudocode --- This is an environment for producing pseudocode for
% algorithms, code, etc.  Keywords can be bolded.  Comments are produced in
% text italic.  Strings are produced in typewriter font.  Each line is
% automatically numbered at its beginning.  Here's a small example:
%
% \begin{pseudocode}
% \L{\K{for} $i$ := 0 \K{to} $n-1$ \K{do}\label{outfor.lin}}
% \L{   \K{for} $j$ := 0 \K{to} $2^{n-1}-1$ \K{do}\label{midfor.lin}}
% \L{      \K{for} $k$ := 0 \K{to} $2^{n-1}-1$ \K{ do}\label{infor.lin}}
% \L{         swap($X[2j,\ 2k+1],\ X[2j+1,\ 2k]$);}
% \L{      \K{endfor}\noline}
% \L{      reverse-shuffle the elements in row $2j$;\label{revsh1.lin}}
% \L{      reverse-shuffle the elements in row $2j+1$;\label{revsh2.lin}}
% \L{   \K{endfor}\noline}
% \L{   reverse-shuffle the rows in $X$;\label{revsh3.lin}}
% \L{\K{endfor}\noline}
% \end{pseudocode}
%
% Note that each line is enclosed by "\L{" and "}."
% \K is used to produce a bolded keyword.
% The line number for a line may be skipped by using \noline at the end.
% The line number may be assigned to a \label at the end.
% \C starts a comment while \CE ends a comment:
%
%    \L{         \C this is a comment \CE}
%
% \S starts a string while \SE finishes it:
%
%    \L{   str = \S "Hello, world.\n" \SE}



% NEW COMMANDS
%
% \setscriptfonts --- Sub and super-script fonts should be no smaller than
% nine points, according to the Thesis Office.  Nine point size is set in
% the frontmatter environment (and usually carries forward) and in the
% \chapter macro; however, it does not make it into some environments.  So,
% \scriptfonts is provided.
%
%
%
% \blankpage --- Produces a "floating" blank page at its point of
% occurrence.  This is useful when you have to perform "pasting" of
% figures, etc. manually.
%
% \datethis --- Writes date and time onto the dvi output.  Useful for
% keeping track of revisions.
%
% \thesisdraft --- When placed in the preamble, causes the phrase "Draft:
% <current date> at <current time>" to be printed in the head of each page.
%
% \useboldmath --- This turns on bold math mode within chapter and section
% titles.  It is not the default because numbers in bold math mode do not
% come out properly.  So, use with discretion.  This should be used in the
% preamble of the LaTeX file.
%
% \vitalsecdepth --- Changes the default depth to which sub-sections are
% numbered and allowed to appear in the table of contents.  By default, the
% depth is 3, corresponding to subsubsections.  To allow only subsections
% to be numbered and appear in the table of contents, use
% "\vitalsecdepth{2}" in the preamble.  Note that the Thesis Office wants
% to see subsubsections numbered.
%
% \tablecaption --- Used in a manner similar to \caption, except that it is
% expressly for table captions.  The caption should appear ABOVE the table.
% Example:
% \tablecaption{Major widget manufacturers}
%
%
%
% The following commands set parameters for the title page of the thesis.
% They are each used in generally the same manner.  For example,
% \author{Thomas P. Kelliher}
%
% \author --- Author of the thesis.
%
% \title --- Title of the thesis, as it should appear on the title page.
% If it runs across several lines, each of the lines save for the last
% should be terminated by "\\".  Example:
% \title{A Study of \\
%        Shrill Cost Accounting \\
%        while Hopping Happily}
%
% \copyrightyear --- Year of copyright of the thesis.
%
% \submitdate --- date of the submission of the thesis.  Actually, the date
% which appears on the title page is the date of GRADUATION.  The format is
% "month, year".  For example, \submitdate{May, 1992}.
%
% \dept --- Department to which thesis is submitted.
%
%
%
% The following commands are for the signature page of the thesis.
%
% \adviser --- Prints "Thesis Adviser".
%
% \coadviser --- Prints "Thesis Co-Adviser".
%
% \chair --- Prints "Chair of Committee".
%
% \cochair --- Prints "Co-Chair of Committee".
%
% \head --- Prints "Head of the Department of <arg>".  Example:
% \head{Computer Science}.
%
% \prof --- Prints "Professor of <arg>".
%
% \assocprof --- Prints "Associate Professor of <arg>".
%
% \assistprof --- Prints "Assistant Professor of <arg>".
%
% \readerone through \readersix --- These are the members of your
% committee.  The ones that are used will appear in order, from one to six.
% The best way to explain them is by starting with examples:
% \readerone{Mary Jane Irwin \\
%            \prof{Computer Science} \\
%            \adviser \\
%            \chair \\
%            \head{Computer Science}}
% 
% \readertwo{Robert Michael Owens \\
%            \assocprof{Computer Science}}
%
% The reader's name should appear first, followed by their title.  Any
% additional titles appear following.  All lines save the last should end
% with "\\".  These should all be defined before using \signaturepage.
%
%
%
% The following deal with the front matter of the thesis.  The general
% structure of this material is as follows:
% 
% \begin{frontmatter}
% <commands to define title and signatory pages>
% \titlepage
% \signaturepage
% \abstract
% <text of abstract>
% \tables
% \preface
% <text of preface>
% \acknowledgments
% <text of acknowledgments>
% \end{frontmatter}
%
% Note that the preface and acknowledgments are optional.
%
% \beginfrontmatter --- Starts the frontmatter by switching to roman
% numbering of pages.  This remains for backward compatibility.  The
% frontmatter environment is preferable.
%
% \titlepage --- Causes the title page of the thesis to be printed.  The
% various parameters (\title, \author, etc.) should have been previously
% defined.
%
% \signaturepage --- Causes the signatory page of the thesis to be printed.
% The readers should have been previously defined.
%
% \abstract --- Produces the header for the abstract of the thesis.  This
% command should be followed by the text of the abstract.
% 
% \tables --- Causes the table of contents, list of tables, and list of
% figures to be printed.
%
% \nolotables --- Causes the list of tables to be omitted from the front
% matter.  Must appear before \tables.
%
% \nolofigures --- Causes the list of figures to be omitted from the front
% matter.  Must appear before \tables.
%
% \preface --- Produces the header for the preface.  It should be followed
% by the text of the preface.  This is optional.
%
% \acknowledgments --- Produces the header for the acknowledgments.  It
% should be followed by the text of the acknowledgments.  This is
% optional also.
%
% \endfrontmatter --- Ends the frontmatter, switching back to arabic
% numbering of pages, etc.  See the note under \beginfrontmatter.
%
% \singleappendix --- This macro is called before the appendix "chapter,"
% assuming you have exactly one appendix.  The appendix follows the
% bibliography.  Example:
% \singleappendix
% \chapter{Survey Data}
% <text of appendix>
%
% \appendices --- This macro is called before the appendix "chapters,"
% assuming you have more than one appendix.  These appendices follow the
% bibliography.  Example:
% \appendices
% \chapter{Stopper Samples}
% <text of appendix>
% \chapter{Widget Samples}
% <text of appendix>
%
% \vita --- This is used to start the vita page, the last page of the
% thesis.  The vita is written in paragraph form and must occupy exactly
% one page.


% For the diligent, additional comments are given with the macros.

% \typeout{Style option `psuthesis', revision 3.0.}
\typeout{}
\typeout{LaTeX 2e document class `psuthesis', S. Simpson, 1996/11/10}
\typeout{THIS IS EXPERIMENTAL !!!!  USE AT YOUR OWN RISK !!!!}
\typeout{}

% Backward compatibility, for those still using "report" as the document
% style and "psuthesis" as a style option.  3/10/93 tpk.

\ifx\undefined\@ptsize
 \input{report.sty}
\fi

% The two point size options are 10 and 11.  12 is not supported.  \ifelvpt
% tells what option is in use.  3/10/93 tpk.

\newif\ifelvpt
\ifcase \@ptsize  \or
                  \elvpttrue
\else\errmessage{Unsupported point size: 1\@ptsize}
\fi

% Page numbers should appear in the upper-righthand corner.

\pagestyle{myheadings}

% If 11pt is in use, assume double spacing and set baselineskip to 12pt,
% which is saner than the 13.6pt used for singlespacing.  The singlespace
% environment knows to increase baselineskip.  3/10/93 tpk.

\ifelvpt
% \def\@normalsize{\@setsize\normalsize{12pt}\xipt\@xipt
 \def\@normalsize{\@setfontsize\normalsize\@xipt{12pt}
 \abovedisplayskip 11pt plus3pt minus6pt\belowdisplayskip \abovedisplayskip
 \abovedisplayshortskip \z@ plus3pt\belowdisplayshortskip 6.5pt plus3.5pt
 minus3pt\let\@listi\@listI} 
\fi

% The thesis is supposed to be double-spaced, so lets bring in that style.
% In cases where you actually need single-spacing, use the singlespace
% environment.

\input{doublespace.sty}

% The following establishes a left margin of slightly more than 1.5" and a 
% right margin of slightly more than 0.5".  Photocopying may cause the text
% to enlarge, shrinking the margins.

\oddsidemargin 40pt
\evensidemargin 10pt
\textwidth 5.8in

% The following are for marginal notes, and are not changed from the
% original values.

\marginparwidth 40pt
\marginparsep 10pt 

% Thesis Office wants indentation of 4-5 characters.  This sets up the
% proper indentation.

\parindent 3em

% Set the top margin and the separation between the page number and the
% first line of text.  Text height is set so as to leave at least slightly
% more than 1" for the bottom margin (same for the top).

\topmargin 4pt
\headsep 15pt
\textheight 8.5in

% Don't allow super- or sub-scripts to get too close to adjacent lines.
% 3/10/93 tpk.

\lineskiplimit=4pt
\lineskip 8pt minus 4pt

% 5/14/92 kelliher
% Save the defaults for several page breaking penalties, so that they can
% restore them within the relaxpagebreaking environment.

\newcount\@brokenpenalty\@brokenpenalty=\brokenpenalty
\newcount\@widowpenalty\@widowpenalty=\widowpenalty
\newcount\@displaywidowpenalty\@displaywidowpenalty=\displaywidowpenalty
\newcount\@predisplaypenalty\@predisplaypenalty=\predisplaypenalty
\newcount\@postdisplaypenalty\@postdisplaypenalty=\postdisplaypenalty

% 5/14/92 kelliher
% The following will prevent most bad page breaks.  The effect can be
% turned off with the relaxpagebreaking environment.

%\def\setbreakpenalties{\brokenpenalty=10000\widowpenalty=10000
%  \clubpenalty=10000\displaywidowpenalty=10000\predisplaypenalty=10000
%  \postdisplaypenalty=10000}
%\setbreakpenalties

\def\setbreakpenalties{\brokenpenalty=10000\widowpenalty=10000
  \clubpenalty=10000}
\setbreakpenalties

% 5/14/92 kelliher
% Change the defaults for where figures and tables are placed.

\def\fps@figure{thbp}
\def\fps@table{thbp}

% A text page must contain at least 50% text.
%% SCG: 40% text 
\renewcommand{\textfraction}{0.4}
\renewcommand{\floatpagefraction}{0.5}

% Figures/tables (floats) must be separated by four lines.  Floats must be
% separated from text by four lines.
% Assume that a line is 12pts.
%%SCG: edit this - this is way too much - floats separated by at least _2_ 11 pt lines.
%\floatsep 48pt
%\textfloatsep 48pt
%\intextsep 48pt
\floatsep 22pt
\textfloatsep 22pt
\intextsep 22pt
% \@maxsep 48pt   % Maximum of \floatsep, \textfloatsep, and \intextsep
\@fpsep \floatsep   % floatsep for float pages

% The \thesisdraft command in the preamble will cause the word "Draft:"
% followed by the current date at time to appear in the header of each page.

\newif\ifthesisdraft
\def\thesisdraft{\thesisdrafttrue\newsavebox{\draftbox}
   \sbox{\draftbox}%
    {\ifelvpt\scriptsize\fi\framebox{{\sc Draft:}\space\datethis}}
   %\markright{\hfil\usebox{\draftbox}}}
   \markboth{\hfil\usebox{\draftbox}}{\hfil\usebox{\draftbox}}}

% \useboldmath turns on bold math mode for chapter and section titles.
% Numbers in bold math mode do not come out properly.

\newif\ifboldmathon % 5/5/1992 kelliher
\def\useboldmath{\boldmathontrue}

% Redefine the font sizes for chapter and section headings.  Bring in the
% nine point fonts for sub and super scripts.

% Bring in the ten pt math italic for 11pt type.  3/10/93 tpk.

% \font\ninerm=cmr9
% \font\ninei=cmmi9
% \font\ninesy=cmsy9
% \ifelvpt\font\teni=cmmi10\fi

% 6/18/92 kelliher The following raises the superscripts and lowers the
% subscripts, making for easier reading given the larger point sizes of the
% script and scriptscript fonts, which are set next.

% Added \elvsy for 11pt type.  3/10/93 tpk.

\DeclareFontShape{OMS}{cmsy}{m}{n}{%
      <5> <6> <7> <8> <9> <10> gen * cmsy
      <10.95> <12> <14.4> <17.28> <20.74> <24.88> cmsy10
      }{
 \fontdimen13\font=1.2\fontdimen13\font % sup1
 \fontdimen14\font=1.2\fontdimen14\font % sup2
 \fontdimen15\font=1.2\fontdimen15\font % sup3
 \fontdimen16\font=1.5\fontdimen16\font % sub1
 \fontdimen17\font=1.5\fontdimen17\font % sub2
 \fontdimen18\font=1.2\fontdimen18\font % sup_drop
 \fontdimen19\font=1.2\fontdimen19\font % sub_drop
}
\DeclareFontShape{OMS}{cmsy}{b}{n}{%
      <5> <6> <7> <8> <9> <10> gen * cmsy
      <10.95> <12> <14.4> <17.28> <20.74> <24.88> cmsy10
      }{
 \fontdimen13\font=1.2\fontdimen13\font % sup1
 \fontdimen14\font=1.2\fontdimen14\font % sup2
 \fontdimen15\font=1.2\fontdimen15\font % sup3
 \fontdimen16\font=1.5\fontdimen16\font % sub1
 \fontdimen17\font=1.5\fontdimen17\font % sub2
 \fontdimen18\font=1.2\fontdimen18\font % sup_drop
 \fontdimen19\font=1.2\fontdimen19\font % sub_drop
}

% \fontdimen13\ninesy=5pt
% \fontdimen14\ninesy=5pt
% \fontdimen15\ninesy=5pt
% \fontdimen16\ninesy=5pt
% \fontdimen17\ninesy=5pt
% \fontdimen13\tensy=5pt
% \fontdimen14\tensy=5pt
% \fontdimen15\tensy=5pt
% \fontdimen16\tensy=5pt
% \fontdimen17\tensy=5pt
% \ifelvpt
%  \fontdimen13\elvsy=6pt
%  \fontdimen14\elvsy=6pt
%  \fontdimen15\elvsy=6pt
%  \fontdimen16\elvsy=6pt
%  \fontdimen17\elvsy=6pt
% \fi

% \setscriptfonts set the fonts for super and sub-scripts to nine or ten
% points, depending upon the normal point size.  Nine point is the minimum
% size acceptable to the Thesis Office.

% Added 11pt support.  3/10/93 tpk.

%\DeclareMathSizes{\@xiipt}{\@xiipt}{\@xipt}{\@xpt}
%\DeclareMathSizes{\@xipt}{\@xipt}{\@xpt}{\@ixpt}
\DeclareMathSizes{\@ixpt}{\@ixpt}{\@ixpt}{\@ixpt}
\def\setscriptfonts{}

% \ifelvpt
%  \def\setscriptfonts{\global\scriptfont0=\tenrm
%                      \global\scriptfont1=\teni
%                      \global\scriptfont2=\tensy
%                      \global\scriptfont3=\tenex
%                      \global\scriptscriptfont0=\ninerm
%                      \global\scriptscriptfont1=\ninei
%                      \global\scriptscriptfont2=\ninesy
%                      \global\scriptscriptfont3=\tenex}
% \else
%  \def\setscriptfonts{\global\scriptfont0=\ninerm
%                      \global\scriptfont1=\ninei
%                      \global\scriptfont2=\ninesy
%                      \global\scriptfont3=\tenex
%                      \global\scriptscriptfont0=\ninerm
%                      \global\scriptscriptfont1=\ninei
%                      \global\scriptscriptfont2=\ninesy
%                      \global\scriptscriptfont3=\tenex}
% \fi

% 7/9/92 kelliher \scriptscriptsize, \scriptsize, and \footnotesize set to
% nine or ten pts.  Baseline skips were wrong for \large through \Huge, fixed
% them.

% Added 11pt support.  3/10/93 tpk.

% \def\scriptscriptsize{\@setsize\scriptscriptsize{11pt}\ixpt\@ixpt}
\def\scriptscriptsize{\@setfontsize\scriptscriptsize\@ixpt{9pt}}
\ifelvpt
% \def\scriptsize{\@setsize\scriptsize{12pt}\xpt\@xpt}
 \def\scriptsize{\@setfontsize\scriptsize\@xpt{10pt}}
\else
 \def\scriptsize{\scriptscriptsize}
\fi
\def\footnotesize{\scriptsize}
\def\tiny{\scriptscriptsize}
% \def\large{\@setsize\large{14pt}\xiipt\@xiipt}
\def\large{\@setfontsize\large\@xiipt{14pt}}
% \def\Large{\@setsize\Large{18pt}\xivpt\@xivpt}
\def\Large{\@setfontsize\Large\@xivpt{18pt}}
% \def\LARGE{\@setsize\LARGE{18pt}\xivpt\@xivpt}
\def\LARGE{\@setfontsize\LARGE\@xivpt{18pt}}
% \def\huge{\@setsize\huge{18pt}\xivpt\@xivpt}
\def\huge{\@setfontsize\huge\@xivpt{18pt}}
% \def\Huge{\@setsize\Huge{18pt}\xivpt\@xivpt}
\def\Huge{\@setfontsize\Huge\@xivpt{18pt}}
\normalsize 

% Redefine the placement of titles for chapters, tables and figures.

% 5/19/92 sanjeev
% Problem with the "appendix" line of a single appendix not being quite
% centered.  This is due to a "hanging" space after the word "Appendix."
% Fixed by making that space part of the following macro, which is "nulled"
% when \singleappendix is called.  This macro is called by
% \@makechapterhead.

\def\@chapnum{\ \thechapter}

% Modified the following so that "Chapter," "Appendix," or whatever appears
% before the number in the table of contents.  3/10/93 tpk.

\def\@chapter[#1]#2{
  \refstepcounter{chapter}
  \typeout{\@chapapp\space\thechapter.}
  \addcontentsline{toc}{chapter}
  {\protect\numberline{\@chapapp\space\thechapter.}#1}
  \chaptermark{#1}
  \addtocontents{lof}{\protect\addvspace{10pt}}
  \addtocontents{lot}{\protect\addvspace{10pt}} \if@twocolumn
  \@topnewpage[\@makechapterhead{#2}] 
  \else \@makechapterhead{#2}
  \@afterheading \fi}

% Modified \@makechapterhead to allow multi-line chapter titles.  Automatic
% linebreaking is turned off because lines are to be broken for grammatical
% sense, not for line filling purposes, according to the Thesis Office.

\def\@makechapterhead#1{{ \parindent 0pt 
 \ifnum \c@secnumdepth >\m@ne 
 \large\bf\centerline{\@chapapp{}\@chapnum} % 5/19/92 kelliher
 \par 
 \vskip 30pt \fi
 \begingroup
 \setstretch{1.3}\Large\bf\ifboldmathon\protect\boldmath\fi % 5/5/1992 kelliher
 \setbox\@tempboxa\hbox{\begin{tabular}{c} #1\end{tabular}}
 \ifdim \wd\@tempboxa >\hsize 
 \errmessage{Chapter title needs to have linebreaks (\noexpand\\)
    inserted by the author} 
 \begin{tabular}{c} #1\end{tabular}
 \else \hbox to\hsize{\hfil\box\@tempboxa\hfil}
 \fi
 \endgroup
 \nobreak \vskip 38pt } }

\def\@makeschapterhead#1{{ \parindent 0pt
% \Large\bf\centerline{#1}\par 
 \normalfont\Large\bf\centerline{#1}\par 
 \nobreak \vskip 40pt } }

% 5/5/1992 kelliher.  The following added boldmath capability to chapter
% titles in the table of contents.

% All bolding removed from the table of contents.  Leaders added to all
% entries in the tables.  3/10/93 tpk.

\def\l@chapter#1#2{\addpenalty{-\@highpenalty} 
 \vskip 1.0em plus 1pt \@tempdima 5.3em \begingroup
 \parindent \z@ \rightskip \@pnumwidth 
 \parfillskip -\@pnumwidth% \bf\ifboldmathon\protect\boldmath\fi
 \leavevmode \advance\leftskip\@tempdima \hskip -\leftskip #1\nobreak
 \leaders\hbox{$\m@th \mkern \@dotsep mu.\mkern \@dotsep mu$}\hfill
 \nobreak\hbox to\@pnumwidth{\hss #2}\par
 \penalty\@highpenalty \endgroup}
\def\l@section{\@dottedtocline{1}{3em}{2.3em}}
\def\l@subsection{\@dottedtocline{2}{5.3em}{3.2em}}
\def\l@subsubsection{\@dottedtocline{3}{8.5em}{4.1em}}
\def\l@paragraph{\@dottedtocline{4}{11.5em}{5em}}
\def\l@subparagraph{\@dottedtocline{5}{13.5em}{6em}}

% Increased the separation between page numbers and titles in the table of
% contents.  3/10/93 tpk.

\def\@pnumwidth{2em}
\def\@tocrmarg{3em}

% Make font style roman for captions.  Make captions conform to the Chicago
% Manual of style (CMS).  \caption should only be used for figures, with the
% caption appearing below the figure.  Captions are separated from the
% floats by two lines.

\long\def\@makecaption#1#2{
 \vskip 22pt 
 \setbox\@tempboxa\hbox{#1.\ \, #2}
 \ifdim \wd\@tempboxa >\hsize {#1.\ \, #2\par} \else \hbox
to\hsize{\hfil\box\@tempboxa\hfil} 
 \fi}

\def\fnum@figure{Fig.\ \thefigure}

% The following \defs create a caption for tables which conforms to CMS.
% They're just hacks of the caption macros.  The caption should appear
% above the table.

\def\tablecaption{\refstepcounter\@captype \@dblarg{\@tablecaption\@captype}}

\long\def\@tablecaption#1[#2]#3{\par\addcontentsline{\csname
  ext@#1\endcsname}{#1}{\protect\numberline{\csname 
  the#1\endcsname}{\ignorespaces #2}}\begingroup
    \@parboxrestore
    \normalsize
    \@maketablecaption{\csname fnum@#1\endcsname}{\ignorespaces #3}\par
  \endgroup}

\long\def\@maketablecaption#1#2{
 \hbox to\hsize{\hfil #1. \hfil}
 \setbox\@tempboxa\hbox{#2}
 \ifdim \wd\@tempboxa >\hsize {#2\par} \else \hbox
to\hsize{\hfil\box\@tempboxa\hfil} 
 \vskip 24pt 
 \fi}

% 6/3/92 kelliher Modify theorem environments to correspond with CMS.  The
% initial "label" (the word "Theorem", or whatever) is set in small caps
% and the number is terminated with a period.

\def\@begintheorem#1#2{\it \trivlist \item[\hskip \labelsep{\sc #1\ #2\@.}]}
\def\@opargbegintheorem#1#2#3{\it \trivlist
      \item[\hskip \labelsep{\sc #1\ #2\ (#3)\@.}]}

% 6/4/92 kelliher Provide the following "theorem" environments.  Note that
% the optional argument causes these environments to be numbered within
% chapters.

\newtheorem{lemma}{Lemma}[chapter]
\newtheorem{corollary}{Corollary}[chapter]
\newtheorem{theorem}{Theorem}[chapter]

% 5/14/92 kelliher
% Page-breaking penalties are set VERY strictly.  This environment will
% return them to their default values so that they may be overridden.

\def\relaxpagebreaking{\clubpenalty=\@clubpenalty\brokenpenalty=\@brokenpenalty
  \widowpenalty=\@widowpenalty\displaywidowpenalty=\@displaywidowpenalty
  \predisplaypenalty=\@predisplaypenalty
  \postdisplaypenalty=\@postdisplaypenalty}
\def\endrelaxpagebreaking{\setbreakpenalties}

% \blankpage will produce a "floating" blank page.

\def\blankpage{\begin{figure}
\vskip\textheight
\end{figure}}

% Stolen from cwebmac.tex
% \datethis to get the date and time on the output.
%
% modified 3/27/92 (sanjeev@cs.psu.edu)
%

\def\today{\ifcase\month\or
  January\or February\or March\or April\or May\or June\or
  July\or August\or September\or October\or November\or December\fi
  \space\number\day, \number\year}

\newcount\twodigits
\newcount\hrs

\def\hours{\twodigits=\time%
  \ifnum\twodigits>719\gdef\ampm{pm}%
  \else\gdef\ampm{am}\fi%
  \divide\twodigits by 60%
  \hrs=\twodigits%
  \ifnum\twodigits=0\twodigits=12\fi%
  \ifnum\twodigits>12\advance\twodigits-12\fi\advance\twodigits0%
  \ifnum\twodigits>9\printtwodigits:\else\printonedigit:\fi%
  \twodigits=\hrs%
  \multiply\twodigits by-60%
  \advance\twodigits by\time%
  \printtwodigits\space\ampm}

\def\gobbleone1{}
\def\gobbleten10{}

\def\printonedigit{%
  \advance\twodigits100
  \expandafter\gobbleten\number\twodigits
  \advance\twodigits-100 }

\def\printtwodigits{%
  \advance\twodigits100
  \expandafter\gobbleone\number\twodigits
  \advance\twodigits-100 }

\def\datethis{\medskip{\sc\today\ at \hours}\bigskip}

% \addcontentsline is modified so that \\ becomes \empty temporarily.  This
% was required so that chapter titles could be broken into multiple lines
% (using \\) by the author.  I don't want the effect of the \\'s to appear
% in the table of contents, so I make the modification.

\def\addcontentsline#1#2#3{%
  \protected@write\@auxout
      {\let\\=\empty 
       \let\label\@gobble \let\index\@gobble \let\glossary\@gobble}%
      {\string\@writefile{#1}%
           {\protect\contentsline{#2}{#3}{\thepage}}}%
}
% \def\addcontentsline#1#2#3{\if@filesw \begingroup \let\\=\empty
% \let\label\@gobble \let\index\@gobble \let\glossary\@gobble
% \def\protect##1{\string\string\string##1\string\space
%    \space}\@temptokena{\thepage}%
% \edef\@tempa{\write \@auxout{\string\@writefile{#1}{\protect
%      \contentsline{#2}{#3}{\the\@temptokena}}}}\@tempa
%    \if@nobreak \ifvmode\nobreak\fi\fi\endgroup\fi}
  
% Introduce indentation after chapter and section headings

% \chapter allows multiple line chapter titles, using \\.  Example:
% \chapter{This is a \\ multiple line  title}
% Moreover, \\ is the ONLY way of getting line breaks.  This is due to
% Thesis Office requirements that titles be broken according to grammatical
% sense.
% 5/19/92 kelliher added \setscriptfonts.

% All pages which may be numbered ARE numbered.  3/10/93 tpk.

\def\chapter{\setscriptfonts\clearpage%\thispagestyle{empty}
\global\@topnum\z@\@afterindenttrue \secdef\@chapter\@schapter}

% The following allows you to change the greatest depth of section headings
% which are numbered and included in the table of contents.  By default
% this depth is 3, which corresponds to subsubsections.  To have only
% subsections included, put "\vitalsecdepth{2}" into you preamble.

\def\vitalsecdepth#1{\setcounter{secnumdepth}{#1}
   \setcounter{tocdepth}{#1}}
\vitalsecdepth{3} % 5/13/92 kelliher

 % 5/5/1992 kelliher.  Added boldmath to sections, subsections, and
 % subsubsections.
\def\section{\@startsection {section}{1}{\z@}{3.5ex plus 1ex minus 
%.2ex}{2.3ex plus .2ex}{\large\bf\ifboldmathon\protect\boldmath\fi}}
 .2ex}{2.3ex plus .2ex}{\normalfont\large\bf\ifboldmathon\protect\boldmath\fi}}
\def\subsection{\@startsection{subsection}{2}{\z@}{3.25ex plus 1ex minus 
% .2ex}{1.5ex plus .2ex}{\bf\ifboldmathon\protect\boldmath\fi}}
 .2ex}{1.5ex plus .2ex}{\normalfont\bf\ifboldmathon\protect\boldmath\fi}}
\def\subsubsection{\@startsection{subsubsection}{3}{\z@}{3.25ex plus
% 1ex minus .2ex}{1.5ex plus .2ex}{\bf\ifboldmathon\protect\boldmath\fi}}
 1ex minus .2ex}{1.5ex plus .2ex}{\normalfont\bf\ifboldmathon\protect\boldmath\fi}}
\def\paragraph{\@startsection
 {paragraph}{4}{\z@}{3.25ex plus 1ex minus .2ex}{-1em}{\bf}}
\def\subparagraph{\@startsection
 {subparagraph}{4}{\parindent}{3.25ex plus 1ex minus 
 .2ex}{-1em}{\bf}}


% Redefine titles for Table of Contents, Tables and Figures.

\def\tableofcontents{\@restonecolfalse\if@twocolumn\@restonecoltrue\onecolumn
 \fi\chapter*{%\thispagestyle{empty}
 Table of Contents\@mkboth{Table of Contents}{Table of Contents}}
 \@starttoc{toc}\if@restonecol\twocolumn\fi}

\def\listoffigures{\@restonecolfalse\if@twocolumn\@restonecoltrue\onecolumn
 \fi\chapter*{%\thispagestyle{empty}
 List of Figures\@mkboth
 {List of Figures}{List of Figures}}\@starttoc{lof}\if@restonecol\twocolumn
 \fi}

\def\listoftables{\@restonecolfalse\if@twocolumn\@restonecoltrue\onecolumn
 \fi\chapter*{%\thispagestyle{empty}
 List of Tables\@mkboth
 {List of Tables}{List of Tables}}\@starttoc{lot}\if@restonecol\twocolumn
 \fi}

%   NOTE [MAS Jun 1, 2005]: these macros don't actually seem to work, 
%   they only change "Bibliography" to "References" in the TOC. So, 
%   I changed "References" back to "Bibliography" to keep it consistant
%   and keep the grad school happy.
% The following two macros were modified to print "References" instead of
% "Bibliography."
%
%\def\bibliography#1{\newpage %\thispagestyle{empty}
% \typeout{References.}
%  \addcontentsline{toc}{chapter}{References}
%  \if@filesw\immediate\write\@auxout{\string\bibdata{#1}}\fi
%  \@input{\jobname.bbl}}
%
%\def\thebibliography#1{\chapter*{References\@mkboth
% {BIBLIOGRAPHY}{BIBLIOGRAPHY}}\list
% {[\arabic{enumi}]}{\settowidth\labelwidth{[#1]}\leftmargin\labelwidth
% \advance\leftmargin\labelsep
% \usecounter{enumi}}
% \def\newblock{\hskip .11em plus .33em minus .07em}
% \sloppy\clubpenalty4000\widowpenalty4000
% \sfcode`\.=1000\relax}

% The following two macros were modified versions of the above 
% commented-out ones [MAS June 1, 2005] to print 
% "Bibliography" instead of "References" so it is consistant in both
% the TOC and at the end.

\def\bibliography#1{\newpage %\thispagestyle{empty}
 \typeout{Bibliography.}
  \addcontentsline{toc}{chapter}{Bibliography}
  \if@filesw\immediate\write\@auxout{\string\bibdata{#1}}\fi
  \@input{\jobname.bbl}}

\def\thebibliography#1{\chapter*{Bibliography\@mkboth
 {BIBLIOGRAPHY}{BIBLIOGRAPHY}}\list
 {[\arabic{enumi}]}{\settowidth\labelwidth{[#1]}\leftmargin\labelwidth
 \advance\leftmargin\labelsep
 \usecounter{enumi}}
 \def\newblock{\hskip .11em plus .33em minus .07em}
 \sloppy\clubpenalty4000\widowpenalty4000
 \sfcode`\.=1000\relax}

% Define an environment for algorithms (pseudo-code)

% Originally Based on Van Jacobson's ``tgrindmac'', a macro package for TeX
% grinding. 

% This environment consists of modifications to tgrind.sty (author
% unknown).

\newif\ifcomment\newif\ifstring\newif\iflinenumber
\newcount\linecount
\newbox\linesofar
\newdimen\TBwid
\newdimen\ts
\newbox\tbox
\newdimen\numfieldwidth

\def\pseudocode{%

\def\@currentlabel{\the\linecount}
\def\noline{\global\linenumberfalse}
\setbox\@tempboxa\hbox{99999.}
\numfieldwidth=\wd\@tempboxa

% Never mind.

\let\Head=\@gobble
\def\File##1,##2,##3{}
\let\Proc=\@gobble
\let\ProcCont=\@gobble

% Each formfeed in the input is replaced by a "\NewPage" macro.  If
% you really want a page break here, define this as "\vfill\eject".

\def\NewPage{\filbreak\bigskip}

% Each line of the program text is enclosed by a "\L{...}".  We turn
% each line into an hbox of size hsize.  If we saw a procedure name somewhere
% in the line (i.e., "procbox" is not null), we right justify "procbox"
% on the line.  Every line we output a small, left justified line number.

\def\L##1{\linenumbertrue\par\setbox\@tempboxa\hbox{\CF\strut##1}
  \hbox to \hsize{\iflinenumber\linebox\else\hbox to\numfieldwidth{} \fi
  \hskip 2em\box\@tempboxa\hss}}

\linecount=1
\def\linebox{\hbox to\numfieldwidth{\hss\the\linecount.}
  \global\advance\linecount by 1}

% The following weirdness is to deal with tabs.  "Pieces" of a line
% between tabs are output as "\LB{...}".  E.g., a line with a tab at
% column 16 would be output as "\LB{xxx}\Tab{16}\LB{yyy}".  (Actually, to
% reduce the number of characters in the .tex file the \Tab macro
% supplies the 2nd & subsequent \LB's.) We accumulate the LB stuff in an
% hbox.  When we see a Tab, we grab this hbox (using "\lastbox") and turn
% it into a box that extends to the tab position.  We stash this box in
% "\linesofar" & use "\everyhbox" to get \linesofar concatenated onto the
% front of the next piece of the line.  (There must be a better way of
% doing tabs [cf., the Plain.tex tab macros] but I'm not not enough of a
% TeX wizard to come up with it.  Suggestions would be appreciated.)

\def\LB{\CF\hbox}
\setbox\linesofar=\null
\everyhbox={\box\linesofar}
\def\Tab##1{\setbox\tbox=\lastbox\TBwid=1\wd\tbox\advance\TBwid by 1\ts
\ifdim\TBwid>##1\ts
\setbox\linesofar=\hbox{\box\tbox\space}\else
\setbox\linesofar=\hbox to ##1\ts{\box\tbox\hfil}\fi\LB}

% A normal space is too thin for code listings.  We make spaces & tabs
% be in "\ts" units (which are the width of a "0" in the current font).

\setbox\tbox=\hbox{0} \ts=1\wd\tbox \setbox\tbox=\hbox{\hskip 1\ts}
%\def\space{\hskip 1\ts\relax}
% This was commented out because it caused bugs.  S. Simpson, 5/21/96.

% Font changing stuff for keywords, comments & strings.  We put keywords
% in boldface, comments in text-italic & strings in typewriter.  Since
% we're usually changing the font inside of a \LB macro, we remember the
% current font in \CF & stick a \CF at the start of each new box.
% Also, the characters " and ' behave differently in comments than in
% code, and others behave differently in strings than in code.

\let\CF=\rm
\def\K##1{{\bf ##1}}    % Keyword
\def\C{\it\global\let\CF=\it\global\commenttrue\relax}  % Comment Start
\def\CE{\rm\global\let\CF=\rm\global\commentfalse\relax}% Comment End
\def\S{\tt\global\let\CF=\tt\global\stringtrue\relax}   % String Start
\def\SE{\rm\global\let\CF=\rm\global\stringfalse\relax} % String End

% Special characters.

\def\{{\ifmmode\lbrace\else\ifstring{\char'173}\else$\lbrace$\fi\fi}
\def\}{\ifmmode\rbrace\else\ifstring{\char'175}\else$\rbrace$\fi\fi}
\def\!{\ifmmode\backslash\else\ifstring{\char'134}\else$\backslash$\fi\fi}
\def\|{\ifmmode|\else\ifstring{\char'174}\else$|$\fi\fi}
\def\<{\ifmmode<\else\ifstring<\else$<$\fi\fi}
\def\>{\ifmmode>\else\ifstring>\else$>$\fi\fi}
\def\/{\ifmmode/\else\ifstring/\else$/$\fi\fi}
\def\-{\ifmmode-\else\ifstring-\else$-$\fi\fi}
\def\_{\ifstring{\char'137}\else\underbar{\ }\fi}
\def\&{{\char'046}}
\def\#{{\char'043}}
\def\%{{\char'045}}
\def\~{{\char'176}}
\def\"{\ifcomment''\else{\tt\char'042}\fi}
\def\'{\ifcomment'\else{\tt\char'047}\fi}
\def\^{{\char'136}}
\def\${{\rm\char'044}}

\raggedright\obeyspaces
\setscriptfonts}

% Redefine the macro used for floats (including figures and tables) so that
% single spacing is used.

\def\@xfloat#1[#2]{\ifhmode \@bsphack\@floatpenalty -\@Mii\else
   \@floatpenalty-\@Miii\fi\def\@captype{#1}\ifinner
      \@parmoderr\@floatpenalty\z@
    \else\@next\@currbox\@freelist{\@tempcnta\csname ftype@#1\endcsname
       \multiply\@tempcnta\@xxxii\advance\@tempcnta\sixt@@n
       \@tfor \@tempa :=#2\do
                        {\if\@tempa h\advance\@tempcnta \@ne\fi
                         \if\@tempa t\advance\@tempcnta \tw@\fi
                         \if\@tempa b\advance\@tempcnta 4\relax\fi
                         \if\@tempa p\advance\@tempcnta 8\relax\fi
         }\global\count\@currbox\@tempcnta}\@fltovf\fi
    \global\setbox\@currbox\vbox\bgroup
    \lineskip\normallineskip\lineskiplimit=1pt
    \def\baselinestretch{1}\@normalsize
%    \boxmaxdepth\z@   % Was commented out in latex.tex 12/15/87!
    \hsize\columnwidth \@parboxrestore}

\long\def\@footnotetext#1{\insert\footins{\lineskip\normallineskip
    \lineskiplimit=1pt
    \def\baselinestretch{1}\footnotesize
    \interlinepenalty\interfootnotelinepenalty 
    \splittopskip\footnotesep
    \splitmaxdepth \dp\strutbox \floatingpenalty \@MM
    \hsize\columnwidth \@parboxrestore
   \edef\@currentlabel{\csname p@footnote\endcsname\@thefnmark}\@makefntext
    {\rule{\z@}{\footnotesep}\ignorespaces
      #1\strut}}}

% Modified so as to do something sane in 11pt.  3/10/93 tpk.

\def\singlespace{%
\vskip\parskip%
\vskip\baselineskip%
\lineskip\normallineskip\lineskiplimit=1pt%
\ifelvpt%
% \def\@normalsize{\@setsize\normalsize{13.6pt}\xipt\@xipt%
\def\@normalsize{\@setfontsize\normalsize\@xipt{13.6pt}%
\abovedisplayskip 11pt plus3pt minus6pt\belowdisplayskip \abovedisplayskip%
\abovedisplayshortskip \z@ plus3pt\belowdisplayshortskip 6.5pt plus3.5pt%
minus3pt\let\@listi\@listI}%
\fi%
\def\baselinestretch{1}%
\ifx\@currsize\normalsize\@normalsize\else\@currsize\fi%
\vskip-\parskip%
\vskip-\baselineskip%
}

% Fiddle with the line skip parameters for providing separation between
% sub- and super-scripts and adjacent lines.  3/10/93 tpk

\def\setstretch#1{\renewcommand{\baselinestretch}{#1}
   \ifcase #1 \typeout{Illegal stretch} \or
              \lineskip\normallineskip\lineskiplimit=1pt  \or
              \lineskip 8pt minus 4pt\lineskiplimit=4pt
   \else
              \typeout{Stretch coefficients outside the range [1.0, 3.0) are
                       not supported.}
              \typeout{I will use doublespacing.}
              \lineskip 8pt minus 4pt\lineskiplimit=4pt
   \fi}

% Redefine the "empty" pagestyle so that the "draft" indication will be
% printed in the head in draft mode.

\def\ps@empty{\let\@mkboth\@gobbletwo\def\@oddfoot{}
\def\@evenfoot{}
\ifthesisdraft
 \def\@oddhead{\hfil\usebox{\draftbox}\hfil}
 \def\@evenhead{\hfil\usebox{\draftbox}\hfil}
\else
 \def\@oddhead{}\def\@evenhead{}
\fi}

% The following should start your front matter.  \setscriptfonts sets the
% fonts for sub and super scripts to nine or ten points.

%\newenvironment{frontmatter}{
\renewenvironment{frontmatter}{
% \def\frontmatter{
   \setscriptfonts
   \pagenumbering{roman}
   }{\newpage\pagenumbering{arabic}}

% \def\beginfrontmatter{\frontmatter}

% \author, \title are defined in report; here is the rest of the front matter.

\def\dept#1{\def\@dept{#1}}
\def\submitdate#1{\def\@submitdate{#1}}
\def\copyrightyear#1{\def\@copyrightyear{#1}}
\def\@title{}\def\@author{}\def\@dept{}
\def\@submitdate{}
\def\@copyrightyear{}

\def\adviser{Thesis Adviser}
\def\coadviser{Thesis Co-Adviser}
\def\chair{Chair of Committee}
\def\cochair{Co-Chair of Committee}
\def\head#1{Head of the Department of #1}
\def\prof#1{Professor of #1}
\def\assocprof#1{Associate Professor of #1}
\def\assistprof#1{Assistant Professor of #1}

\def\readerone#1{\def\@readerone{#1}}
\def\readertwo#1{\def\@readertwo{#1}}
\def\readerthree#1{\def\@readerthree{#1}}
\def\readerfour#1{\def\@readerfour{#1}}
\def\readerfive#1{\def\@readerfive{#1}}
\def\readersix#1{\def\@readersix{#1}}
\def\readerseven#1{\def\@readerseven{#1}}
\def\addsigpage#1{\def\@addsigpage{#1}}

\def\@readerone{}
\def\@readertwo{}
\def\@readerthree{}
\def\@readerfour{}
\def\@readerfive{}
\def\@readersix{}
\def\@readerseven{}
\def\@addsigpage{}

\def\signrule{\vskip 0.75in\hbox{\vrule width 3.5in height 0.4pt \hskip 0.3in
 \vrule width 2in height 0.4pt}}

\def\signaturepage{
\vbox{
    \thispagestyle{empty}
    \setcounter{page}{0}
    \begin{singlespace}
    \hbox{We approve the thesis of \@author .}
    \vskip 0.35in 
    \hbox{\null\hskip 3.9in Date of Signature}
    \ifx\@empty\@readerone \else \signrule
      \raggedright\begin{tabular}{l}\@readerone\end{tabular}\par
    \vskip -0.2in
    \fi
    \ifx\@empty\@readertwo \else \signrule
      \raggedright\begin{tabular}{l}\@readertwo\end{tabular}\par
    \vskip -0.2in
    \fi
    \ifx\@empty\@readerthree \else \signrule
      \raggedright\begin{tabular}{l}\@readerthree\end{tabular}\par
    \vskip -0.2in
    \fi
    \ifx\@empty\@readerfour \else \signrule
      \raggedright\begin{tabular}{l}\@readerfour\end{tabular}\par
    \vskip -0.2in
    \fi
    \ifx\@empty\@readerfive \else \signrule
      \raggedright\begin{tabular}{l}\@readerfive\end{tabular}\par
    \vskip -0.2in
    \fi
    \ifx\@empty\@readersix \else \signrule
      \raggedright\begin{tabular}{l}\@readersix\end{tabular}\par
    \vskip -0.2in
    \fi
    \ifx\@empty\@readerseven \else \signrule
      \raggedright\begin{tabular}{l}\@readerseven\end{tabular}\par
    \fi
    \end{singlespace}
    \typeout{Signature page.}
  
} \newpage }


% 5/19/92 sanjeev
% \titlepage now "automagically" adjusts the height of the title page to
% accommodate titles of various numbers of lines while filling the entire
% page.

\def\titlepage{
  \thispagestyle{empty}
   \vbox to \vsize{\vss\vss
    \centerline{The Pennsylvania State University}
    \centerline{The Graduate School}
    \centerline{Department of \@dept}
    \vss\vss\vss
%    \begingroup\Large\begin{center}{\@title}\end{center}\endgroup
    \begingroup\normalfont\Large\begin{center}{\@title}\end{center}\endgroup
    \vss\vss\vss
    \centerline{A Thesis in}
    \centerline{\@dept}
    \centerline{by}
    \centerline{\@author}
    \vskip0.1in plus 0.1in minus 0.1in
    \centerline{\copyright\ \@copyrightyear\ \@author} 
    \vskip0.2in plus 0.2in minus 0.2in
    \begin{singlespace}
    \centerline{Submitted in Partial Fulfillment}
    \centerline{of the Requirements}
    \centerline{for the Degree of}
    \end{singlespace}
    \vskip0.2in plus 0.2in minus 0.2in
    \centerline{Doctor of Philosophy}
    \vskip0.1in plus 0.1in minus 0.1in
    \centerline{\@submitdate}
    \vss\vss}
    \typeout{Title page.}
    \newpage}

\def\committeepage{
\vbox{
    \thispagestyle{empty}
    \begin{singlespace}
    \hbox{The thesis of \@author\ was read and approved\footnotemark\ by the following:}
%    \vskip 0.35in 
    \vskip 0.75in
    \ifx\@empty\@readerone \else \hskip 0.5in 
      \raggedright\begin{tabular}{l}\@readerone\end{tabular}\par
    \vskip 0.55in
    \fi
    \ifx\@empty\@readertwo \else \hskip 1in
      \raggedright\begin{tabular}{l}\@readertwo\end{tabular}\par
    \vskip 0.55in
    \fi
    \ifx\@empty\@readerthree \else \hskip 1in
      \raggedright\begin{tabular}{l}\@readerthree\end{tabular}\par
    \vskip 0.55in
    \fi
    \ifx\@empty\@readerfour \else \hskip 1in
      \raggedright\begin{tabular}{l}\@readerfour\end{tabular}\par
    \vskip 0.55in
    \fi
    \ifx\@empty\@readerfive \else \hskip 1in
      \raggedright\begin{tabular}{l}\@readerfive\end{tabular}\par
    \vskip 0.55in
    \fi
    \ifx\@empty\@readersix \else \hskip 1in
      \raggedright\begin{tabular}{l}\@readersix\end{tabular}\par
    \vskip 0.55in
    \fi
    \ifx\@empty\@readerseven \else \hskip 1in
      \raggedright\begin{tabular}{l}\@readerseven\end{tabular}\par
    \fi
    \end{singlespace}
    \typeout{Committee page.}
  
}  \footnotetext{Signatures on file in the Graduate School.} \newpage }

% Follow \abstract with the text of the abstract.

\def\abstract{
   \newpage%\thispagestyle{empty}
   \chapter*{Abstract}
   \pagestyle{myheadings}\typeout{Abstract.}}

% \tables produces table of contents, list of tables, etc.  Just say
% \tables.

% \nolotables and \nolofigures prevent the creation of the list of tables
% and list of figures, respectively.

\newif\ifmakelotables
\newif\ifmakelofigures
\makelotablestrue
\makelofigurestrue

\def\nolotables{\makelotablesfalse}
\def\nolofigures{\makelofiguresfalse}

\def\tables{\newpage
   \pagestyle{myheadings} 
   \typeout{Tables.}
   \tableofcontents
   \ifmakelotables
    \newpage
    \addcontentsline{toc}{chapter}{List of Tables}
    \listoftables
   \fi
   \ifmakelofigures
    \newpage
    \addcontentsline{toc}{chapter}{List of Figures}
    \listoffigures
   \fi
   %\newpage
   }

% Follow \preface with the text of the preface.

\def\preface{
   \newpage%\thispagestyle{empty}
   \chapter*{Preface}
   \pagestyle{myheadings}
   \addcontentsline{toc}{chapter}{Preface}\typeout{Preface.}}

% Follow \acknowledgments with the text of the acknowledgments.

\def\acknowledgments{
   \newpage%\thispagestyle{empty}
   \chapter*{Acknowledgments}
   \pagestyle{myheadings}
   \addcontentsline{toc}{chapter}{Acknowledgments}\typeout{Acknowledgments.}}

% This should end the frontmatter.

% \def\endfrontmatter{\newpage\pagenumbering{arabic}}

% 5/18/92 kelliher
% The bibliography is the first component of the backmatter.  Appendices,
% if present, form the second component.  Appendices are begun with the
% \chapter macro; however, either the \singleappendix or \appendices
% command should be given before any of the actual appendices.  Use
% \singleappendix when there is only a single appendix, for multiple
% appendices use \appendices.

% \singleappendix "redefs" \@chapter so that the proper entry is made in
% the table of contents.  Basically, the appendix should not be "numbered."

% \def\singleappendix{\appendix\def\@chapnum{}
%   \def\@chapter[##1]##2{
%     \refstepcounter{chapter}
%     \typeout{\@chapapp.}
%     \addcontentsline{toc}{chapter}{##1}
%     \chaptermark{##1}
%     \addtocontents{lof}{\protect\addvspace{10pt}}
%     \addtocontents{lot}{\protect\addvspace{10pt}} \if@twocolumn
%     \@topnewpage[\@makechapterhead{##2}] 
%     \else \@makechapterhead{##2}
%     \@afterheading \fi}}

% modified 11/10/96 by S. Simpson (to say "Appendix" in the TOC)

\def\singleappendix{\appendix\def\@chapnum{}
  \def\@chapter[##1]##2{
    \refstepcounter{chapter}
    \typeout{\@chapapp.}
    \addcontentsline{toc}{chapter}{\@chapapp.\ \ ##1}
    \chaptermark{##1}
    \addtocontents{lof}{\protect\addvspace{10pt}}
    \addtocontents{lot}{\protect\addvspace{10pt}} \if@twocolumn
    \@topnewpage[\@makechapterhead{##2}] 
    \else \@makechapterhead{##2}
    \@afterheading \fi}}

% \def\appendices{\appendix}

% modified 11/10/96 by S. Simpson (to improve spacing)

\def\appendices{\appendix
  \def\@chapter[##1]##2{
    \refstepcounter{chapter}
    \typeout{\@chapapp\space\thechapter.}
    \addcontentsline{toc}{chapter}%
    {\protect\numberline{\@chapapp\space\thechapter}.\ \ ##1}
    \chaptermark{##1}
    \addtocontents{lof}{\protect\addvspace{10pt}}
    \addtocontents{lot}{\protect\addvspace{10pt}} \if@twocolumn
    \@topnewpage[\@makechapterhead{##2}] 
    \else \@makechapterhead{##2}
    \@afterheading \fi}}

% 5/14/92 kelliher
% The following starts the vita page.  Follow \vita with the text of the
% vita.  The standard form for the thesis vita is just to write it as a
% paragraph or two.  It must fit onto one page.

\def\vita{\newpage\thispagestyle{empty}%
% \begingroup\Large\bf\centerline{Vita}\endgroup\nobreak\vskip 30pt
\begingroup\normalfont\Large\bf\centerline{Vita}\endgroup\nobreak\vskip 30pt
  \typeout{Vita.}}
