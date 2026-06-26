\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{Algo_template}

\DeclareOption*{\edef\hwnum{\CurrentOption}}
\ProcessOptions\relax

\LoadClass[12pt]{extarticle}

\RequirePackage{polyglossia}
\RequirePackage{amsmath}
\RequirePackage{amssymb}
\RequirePackage{amsfonts}
\RequirePackage{graphicx}
\RequirePackage{systeme}
\RequirePackage{mathtools}
\RequirePackage[makeroom]{cancel}
\RequirePackage[a4paper,margin=1in]{geometry}
\usepackage{tikz}
\usepackage{float}
\usepackage{fontspec} 
\usepackage{listings}
\usepackage{xcolor} 
\lstset{
    language=Lisp,
    basicstyle=\ttfamily\small,
    keywordstyle=\color{blue}\bfseries, 
    morekeywords={CAR, CDR, CADR, CADDR, CONS, EVAL, EQ, LIST}, 
    commentstyle=\color{gray},
    stringstyle=\color{red},
    showstringspaces=false,
    breaklines=true,
    frame=single,
    numbers=left,
    numberstyle=\tiny\color{gray},
    tabsize=2
}

\usepackage{mathtools}
\DeclarePairedDelimiter\ceil{\lceil}{\rceil}
\DeclarePairedDelimiter\floor{\lfloor}{\rfloor}

\newcommand{\rnum}[1]{\uppercase\expandafter{\romannumeral #1\relax}}

\DeclarePairedDelimiter{\norm}{\lVert}{\rVert} 

\setdefaultlanguage{english}
\setotherlanguage{hebrew}  
\newfontfamily\hebrewfont[Script=Hebrew]{David CLM}

\title{
    \begin{hebrew}
    שפות תכנות
    \\ 
    תרגיל בית \hwnum
    \end{hebrew}
}
\author{
    \texthebrew{דרה גלוזמן 216415331} \\
    \texthebrew{נבו יצחק 216014639}
}
\date{}

\AtBeginDocument{%
    \AddToHook{begindocument/end}{%
        \maketitle
    }
}