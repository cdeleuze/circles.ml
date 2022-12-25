(*p

\usepackage[francais]{babel}
\usepackage{a4wide}
\usepackage{graphicx}
\usepackage{float}
\usepackage{pst-all}
\usepackage{pst-node}
\usepackage{amsmath}
\usepackage{url}
\newcommand{\bibfieldurl}[1]{ Page web : {\small\url{#1}}.}

\newcommand{\img}[2]{\includegraphics[width=#1]{#2}}
\newenvironment{figureH}{\begin{figure}[h]}{\end{figure}}
\newenvironment{figureh}{\begin{figure}[h]}{\end{figure}}
\NoAutoSpaceBeforeFDP
\newcommand{\vect}[1]{\overrightarrow{#1}}

\title{Empilements de cercles}
\author{Christophe Deleuze}
\date{26 ao�t 2007}
\AtBeginDocument{\maketitle\begin{abstract}

    La rubrique ``Formes math�matiques'' d'un r�cent num�ro de la
    revue \emph{D�couverte} (revue du palais de la d�couverte)
    \cite{empilements_de_cercles} pr�sente les figures math�matiques
    appel�es ``empilements de cercles''.  Ce programme, �crit en
    \emph{Objective Caml} \cite{ocaml}, permet de r�aliser de telles
    figures.

    Il a �t� r�dig� dans le style ``programmation litt�raire''
    \emph{(literate programming)}, qui int�gre la documentation et le
    code source en un seul document.  L'outil utilis� est
    \textsf{Ocamlweb} \cite{ocamlweb}, qui permet de m�ler code en
    Caml et documentation en \LaTeX{}.

\end{abstract}}

*)

(*s

Un empilement de cercles est un ensemble de cercles tangents ou
disjoints, construits de mani�re syst�matique comme sur la figure
\ref{fig:exemple}.

\begin{figureh}
  \centering
    % 100 75 50 2
  \img{9cm}{exemple}
  \caption{Un empilement de cercles}
  \label{fig:exemple}
\end{figureh}

*)

(*s

Un th�or�me de Ren� Descartes �nonce que le carr� de la somme des
courbures (la courbure d'un cercle est d�finie comme l'inverse de son
rayon) de quatre cercles deux � deux tangents est �gal au double de la
somme de leurs carr�s :

$$ 2(e^2+f^2+g^2+h^2) = (e+f+g+h)^2 $$

Ainsi, si nous connaissons les courbures [e], [f] et [g] de trois
cercles tangents deux � deux, nous devons r�soudre une simple �quation
du second degr� pour d�terminer la courbure d'un quatri�me cercle
tangent aux trois autres.  Il y a en fait toujours deux cercles
possibles, il y aura donc toujours des solutions (deux distinctes ou
une double dans le cas o� les deux cercles solutions ont la m�me
courbure).  Une courbure peut �tre n�gative : cela signifie que le
cercle en question englobe les autres, comme c'est le cas du grand
cercle de la figure \ref{fig:exemple}.  La fonction ci-dessous calcule
les deux courbures en question.

%% possibilit� sch�ma pour illustrer courbure positive/n�gative

*)

let solve_real e f g =
  let delta = 16. *. (e*.f+.e*.g+.f*.g) in
  let sqd = sqrt delta
  in
  (2.*.(e+.f+.g)-.sqd)/.2., (2.*.(e+.f+.g)+.sqd)/.2.

(*s

Une g�n�ralisation de cette relation de Descartes s'applique aux
nombres complexes $c_iz_i$, produits de la courbure $c_i$ et du
complexe repr�sentant le centre du cercle $z_i$.

$$ 2((c_ez_e)^2+(c_fz_f)^2+(c_gz_g)^2+(c_hz_h)^2) =
(c_ez_e+c_fz_f+c_gz_g+c_hz_h)^2 $$

En la r�solvant de la m�me fa�on, on obtient le produit complexe
courbure fois position du centre pour chacun des deux cercles
solutions.  Pour rendre les expressions lisibles les op�rateurs
arithm�tiques sur les entiers sont red�finis provisoirement pour
manipuler les complexes.

*)

let solve_complex e f g =
  let c16 = { Complex.re=16.;Complex.im=0.}
  and c2  = { Complex.re=2.; Complex.im=0.}
  in
  let ( * ) = Complex.mul and ( / ) = Complex.div
  and ( + ) = Complex.add and ( - ) = Complex.sub
  in
  let delta = c16 * (e*f + e*g + f*g) in
  let sqd = Complex.sqrt delta
  in
  (c2*(e+f+g)-sqd) / c2, (c2*(e+f+g)+sqd) / c2

(*s

Nous pouvons donc calculer d'une part les deux courbures [ca] et [cb],
d'autre part les deux produits courbure fois position du centre [cz1]
et [cz2].  Il nous reste � associer les deux pour conna�tre les deux
cercles solutions.

Par d�finition, la distance de chacun des deux centres au centre de
l'un quelconque des trois cercles initiaux, le cercle de r�f�rence
[(zr,cr)], est �gale au rayon du cercle de r�f�rence plus le rayon du
cercle construit (c'est � dire l'inverse de sa courbure).

La phrase pr�c�dente est vraie si les deux courbures sont positives.
Si l'une est n�gative (cercle ``contenant'' l'autre) les rayons
doivent �tre soustraits.  Si nous consid�rons que les rayons peuvent
�tre n�gatifs (inverse de la courbure), l'op�ration reste une addition
(par contre le r�sultat peut �tre n�gatif et il faudra en prendre la
valeur absolue).

% \begin{figureH}
%   \centering
%   \psset{unit=7mm}
%   \hfill
%   \begin{pspicture}(-5,0)(5,5)
%     \pscircle(0,2){2}
%     \pcline(0,2)(0,4)
%     \Aput{dd}

% %     \pnode(0,2){zr}
% %     \pnode(2,4){cr}
% %     \ncline{->}{zr}{cr}
% %     \Aput{$1/c_r$}

    
%   \end{pspicture}
%   \hfill
%   \begin{pspicture}(-5,0)(5,5)
    
%   \end{pspicture}
%   \hfill~
%   \caption{Relations entre rayons des cercles tangents}
%   \label{fig:associate}  
% \end{figureH}

Nous calculons les deux points [z1a] et [z2a] correspondant aux deux
choix de centres possibles pour la courbure [ca], calculons les
distances avec [zr] (ce sont les modules des complexes [z1a]$-$[zr] et
[z1b]$-$[zr]) et y soustrayons la distance th�orique
$|\frac{1}{ca}+\frac{1}{cr}|$.  La diff�rence devrait �tre nulle pour
l'un des deux points.  Pour tenir compte des erreurs d'arrondis nous
choisissons celui pour lequel elle est la plus faible en valeur absolue.

Nous pouvons renvoyer les valeurs des deux cercles correspondants.
Dans la suite, un cercle sera toujours repr�sent� par un couple
(complexe, r�el) repr�sentant son centre et sa courbure.

*)


let cdiv { Complex.re=re; Complex.im=im } r =
  { Complex.re=re/.r; Complex.im=im/.r }

let associate_center_and_curve cz1 cz2 ca cb (zr,cr) =
  let z1a = cdiv cz1 ca
  and z2a = cdiv cz2 ca
  and ( - ) = Complex.sub
  in
  let d1 = abs_float((Complex.norm (z1a-zr)) -. (abs_float (1./.cr+.1./.ca)))
  and d2 = abs_float((Complex.norm (z2a-zr)) -. (abs_float (1./.cr+.1./.ca)))
  in
  if d1<d2 then (z1a, ca), (cdiv cz2 cb, cb)
  else (z2a, ca), (cdiv cz1 cb, cb)


(*s

�tant donn�s trois cercles tangents deux � deux, nous avons maintenant
tout ce qu'il nous faut pour d�terminer les deux cercles qui sont
tangents � ces trois cercles.

*)

let cmul { Complex.re=re; Complex.im=im } r =
  { Complex.re=re*.r; Complex.im=im*.r }

let get_circles (z1,c1) (z2,c2) (z3,c3) =
  let ca,cb = solve_real c1 c2 c3
  and cz1,cz2 = solve_complex (cmul z1 c1) (cmul z2 c2) (cmul z3 c3)
  in
  associate_center_and_curve cz1 cz2 ca cb (z1,c1)
;;


(*s

Nous avons dit qu'il y a deux cercles solutions.  Si une seule nous
int�resse, comment les distinguer ?  Consid�rons d'abord le cas o� les
courbures des cercles initiaux $C_1$, $C_2$ et $C_3$ sont toutes
positives.  L'un des deux cercles solutions est \emph{int�rieur},
``coinc�'' � l'int�rieur des trois cercles, l'autre est
\emph{ext�rieur}.  Dans la figure \ref{fig:cpos} � gauche, le cercle
ext�rieur ($C_e$) englobe les trois autres et a donc une courbure
n�gative, mais ce n'est pas forc�ment le cas, comme le montre la
figure \ref{fig:cpos} � droite.  Par contre, on peut facilement se
convaincre (et d�montrer si n�cessaire) que le cercle int�rieur
($C_i$) a toujours le rayon le plus petit, et donc que sa courbure est
la plus grande.

\begin{figureh}
  \centering
% 0.000000 0.000000 30.000000 0.033333
% 55.000000 0.000000 25.000000 0.040000
% 31.818182 -38.569461 20.000000 0.050000
% 24.974073 -3.954382 -55.285202 -0.018088
% 30.343371 -14.838162 3.777081 0.264755
  \psset{unit=4mm}
  \hfill
  \begin{pspicture}(-4,-5)(5,5)
    \pscircle(0,0){3}      \put(-2,1.5){$C_1$}
    \pscircle(5.5,0){2.5}  \put(6,0){$C_2$}
    \pscircle(3.182,-3.857){2} \put(1.5,-4){$C_3$}
    \psset{linewidth=0.03mm}
    \pscircle(2.497,-0.395){-5.528}\put(7,4){$C_e$}
    \pscircle(3.03,-1.484){0.378} \put(1.2,-1.2){$C_i$}
  \end{pspicture}
  \hfill
% 0.000000 0.000000 30.000000 0.033333
% 35.000000 0.000000 5.000000 0.200000
% 44.285714 -23.211538 20.000000 0.050000
% 86.996703 66.982088 79.795384 0.012532
% 35.000000 0.000000 5.000000 0.200000
  \psset{unit=3.5mm}
  \begin{pspicture}(-3,-6)(13,6)
    \pscircle(0,0){3}\put(-1,1.5){$C_1$}
    \pscircle(2.9,-2){0.5} \put(3.2,-1.5){$C_2$}
    \pscircle(2.4,-4.9){2.5}\put(1,-5){$C_3$}
    \psset{linewidth=0.03mm}
    \pscircle(8,0){5}\put(9,3){$C_e$}
    \pscircle(2.25,-2.2){0.22}\put(0.8,-2){$C_i$}
  \end{pspicture}
  \hfill~
  \caption{Position des solutions si les trois cercles ont une courbure positive}
  \label{fig:cpos}
\end{figureh}

Dans la suite du programme [ci] d�signera la courbure du cercle $C_i$
alors que [c_i] d�signera le cercle lui m�me, c'est � dire le couple
[(zi,ci)].

*)


let out_circle c_1 c_2 c_3 =
  let (za,ca), (zb,cb) = get_circles c_1 c_2 c_3
  in
  if ca<cb then (za,ca) else (zb,cb)

let in_circle c_1 c_2 c_3 =
  let (za,ca), (zb,cb) = get_circles c_1 c_2 c_3
  in
  if ca>cb then (za,ca) else (zb,cb)


(*s

Un deuxi�me cas est celui o� l'un des cercles initiaux a une courbure
n�gative ($C_3$ sur la figure \ref{fig:cneg}).  Les deux solutions
$C_g$ et $C_d$ ont alors toujours une courbure positive, comme le
montre la figure.  Nous ferons en sorte que le cercle � courbure
n�gative soit toujours le troisi�me et, en posant par convention que
$C_2$ est au dessus de $C_1$, nous pourrons dire que l'une des deux
solutions ($C_d$) est ``� droite'' et l'autre ($C_g$) ``� gauche'' des
cercles $C_1$ et $C_2$.

\begin{figureh}
  \centering
% 0.000000 0.000000 35.000000 0.028571
% 85.000000 0.000000 50.000000 0.020000
% 27.941176 -69.600939 40.000000 0.025000
% 51.124876 -23.755173 -91.374295 -0.010944
% 93.019326 -74.862181 25.290475 0.039541
% 31.488487 44.970305 19.898572 0.050255
  \psset{unit=3mm}
  \begin{pspicture}(-1,-11)(14.5,6.5)
    \pscircle(0,0){3.5}\put(-1,1.5){$C_1$}
    \pscircle(8.5,0){5}\put(9,3){$C_2$}
    \pscircle(5.112,-2.375){9.137}\put(-6,-3){$C_3$}
    \psset{linewidth=0.03mm}
    \pscircle(2.794,-6.96){4} \put(3,-5){$C_d$}
    \pscircle(3.149,4.497){1.9898}\put(2,5){$C_g$}
%    \pscircle(2.9,-2){0.5}
  \end{pspicture}
  \caption{Position des solutions si l'un des trois cercles ($C_3$) a une
    courbure n�gative}
  \label{fig:cneg}
\end{figureh}

Nous commen�ons par d�finir une fonction qui nous indique si un point
de coordonn�es $z_m$ est � droite des deux points de coordonn�es $z_a$
et $z_b$.  C'est le cas si l'angle $\widehat{ABM}$ est compris entre 0
et $\pi$, c'est � dire de sinus positif.  Cet angle est la diff�rence
entre les angles des vecteurs $\vect{BM}$ et $\vect{BA}$.  Nous
l'utilisons ensuite dans la fonction [right_circle] qui nous renvoie
donc le cercle ``de droite''.

*)

let pi = acos(-1.)

(*i
C'est le cas si l'angle $\widehat{AMB}$ est n�gatif.

let on_right zm za zb =
   let ta = Complex.arg (Complex.sub za zm)
   and tb = Complex.arg (Complex.sub zb zm)
   in
   cos (tb-.ta) < 0.

  let ang = 
     let a = mod_float (tb-.ta) (2.*.pi) in
     if a<(-.pi) then a+.2.*.pi else a
   in
   ang<0.
i*)

let on_right zm za zb =
  let th_bm = Complex.arg (Complex.sub zm zb)
  and th_ba = Complex.arg (Complex.sub za zb)
  in
  sin (th_bm -. th_ba) >= 0.

let right_circle (z1,c1) (z2,c2) (z3,c3) =
  let (za,ca), (zb,cb) = get_circles (z1,c1) (z2,c2) (z3,c3)
  in
  if on_right za z1 z2 then (za,ca) else (zb,cb)


(*s

D�finissons maintenant comment construire r�cursivement notre figure.
Nous partons de trois cercles $C_1$, $C_2$ et $C_3$ tangents deux �
deux et d'un rayon minimal [min] en de�� duquel nous stopperons la
r�cursion.  Consid�rons tout d'abord que tous trois sont � courbure
positive.  Nous voulons construire la liste des cercles qui prennent
place dans l'espace d�limit� par $C_1$, $C_2$ et $C_3$.

Nous savons calculer le cercle $C_4$, que nous avons d�fini
pr�c�demment comme le cercle \emph{int�rieur}.  Notons que ce cercle
est le plus grand de tous les cercles que pourra contenir notre
espace.  Une fois ce cercle construit, il reste � remplir les trois
espaces d�limit�s respectivement par $C_1$, $C_4$, $C_3$, par $C_4$,
$C_2$, $C_3$ et par $C_1$, $C_2$, $C_4$.  Ces espaces sont not�s A, B
et C sur la figure \ref{fig:rec1}.

Nous pouvons donc d�finir notre liste r�cursivement :
\begin{itemize}
\item cas de base : si l'espace est trop petit (le rayon de $C_4$ est
  inf�rieur au rayon [min]), il ne contiendra aucun cercle, la r�ponse
  est donc la liste vide,
\item sinon l'espace contiendra le cercle $C_4$, ainsi que
  les cercles remplissant les espaces d�limit�s par $C_1$, $C_4$,
  $C_3$, par $C_4$, $C_2$, $C_3$ et par $C_1$, $C_2$, $C_4$.
\end{itemize}

\begin{figureh}
  \centering
  \clipbox{\begin{pspicture}(0,-5)(10,2)
  \psset{unit=16mm}
    \pscircle(0,0){3}      \put(2.5,-0.5){$C_1$}
    \pscircle(5.5,0){2.5}  \put(3.1,0){$C_2$}
    \pscircle(3.182,-3.857){2} \put(2,-2.5){$C_3$}
    \psset{linewidth=0.03mm}
%    \pscircle(2.497,-0.395){-5.528}\put(7,4){$C_e$}
    \pscircle(3.03,-1.484){0.378} \put(2.8,-1.5){$C_4$}
    \put(2.87,-1){C}
    \put(2.48,-1.86){A}
    \put(3.41,-1.8){B}
  \end{pspicture}}
\caption{Construction r�cursive pour trois cercles � courbure positive}
\label{fig:rec1}
\end{figureh}

Nous devons aussi traiter le cas des espaces d�limit�s par trois
cercles dont l'un a une courbure n�gative.  Consid�rons qu'il s'agit
de $C_3$.  Comment remplir l'espace ``de droite'' ?  De mani�re tr�s
similaire au cas pr�c�dent : nous calculons d'abord $C_d$ le cercle
``de droite'' puis devons calculer les cercles entrant dans les
espaces d�limit�s par $C_1$, $C_d$ et $C_3$ (A sur la figure
\ref{fig:rec2}), par $C_d$, $C_2$ et $C_3$ (B) et $C_1$, $C_2$ et
$C_d$ (C).  Les zones A et B sont trait�es en constuisant le cercle
``de droite'' alors que la zone C est trait�e en construisant le
cercle ``int�rieur''.


\begin{figureh}
    \centering
  \psset{unit=3mm}
  \begin{pspicture}(-1,-11)(14.5,6.5)
    \pscircle(0,0){3.5}\put(-1,1.5){$C_1$}
    \pscircle(8.5,0){5}\put(9,3){$C_2$}
    \pscircle(5.112,-2.375){9.137}\put(-6,-3){$C_3$}
    \psset{linewidth=0.03mm}
    \pscircle(2.794,-6.96){4} \put(3,-5){$C_d$}
%    \pscircle(3.149,4.497){1.9898}\put(2,5){$C_g$}
    \put(-2.6,-5.5){A}
    \put(2.9,-2.6){C}
    \put(9,-6.5){B}
  \end{pspicture}
\caption{Construction r�cursive dans le cas o� $C_3$ a une courbure n�gative}
\label{fig:rec2}
\end{figureh}


Ne mais doit-on pas remplir aussi l'espace ``de gauche'' ?  Pas
maintenant, remarquez que le cercle de gauche de $C_d$, $C_1$ et $C_3$
existe d�j�, c'est $C_2$, de m�me que $C_1$ pour $C_2$, $C_d$ et
$C_3$.

Nous pouvons fusionner les deux cas.  �tant donn�s trois cercles
$C_1$, $C_2$ et $C_3$, avec $C_3$ ayant �ventuellement une courbure
n�gative nous calculons un quatri�me cercle $C_4$ (l'int�rieur si
$C_3$ a une courbure positive, celui de droite dans le cas contraire)
et proc�dons r�cursivement sur les triplets de cercles :

\begin{itemize}
\item $C_1$, $C_4$ et $C_3$,
\item $C_4$, $C_2$ et $C_3$,
\item $C_1$, $C_2$ et $C_4$.
\end{itemize}

Notons qu'ici nous posons qu'au d�part seul $C_3$ est susceptible
d'avoir une courbure n�gative, ce qui sera maintenu dans la r�cursion
puisqu'il se trouve toujours en derni�re position dans les appels
r�cursifs.

 *)

let rec recurse c_1 c_2 c_3 min =
  let z3,c3 = c_3 in
  let c_4 = if c3>0. then in_circle c_1 c_2 c_3
  else right_circle c_1 c_2 c_3
  in
  let r = match c_4 with (_,c) -> 1./.c
  in if r>min then c_4 :: 
    recurse c_1 c_4 c_3 min @
    recurse c_4 c_2 c_3 min @
    recurse c_1 c_2 c_4 min
  else
    []

(*s

Et voici comment amorcer la r�cursion.  Nous avons trois cercles
$C_1$, $C_2$ et $C_3$ tangents deux � deux, tous � courbure positive
et nous voulons construire l'empilement de cercles d�termin� par ces
trois cercles.

Il nous faut tout d'abord d�terminer le cercle $C_e$ englobant ces
trois.  S'il n'existe pas (le cercle externe est � courbure positive)
nous abandonnons.\footnote{Nous pourrions en fait construire le cercle
  englobant les trois plus gros, qui sera lui de courbure n�gative.}

Sinon notre empilement est constitu� de ces quatre cercles, plus ceux
qui seront construits dans les espaces d�limit�s par (figure \ref{fig:start}) :
\begin{itemize}
\item $C_1$, $C_2$ et $C_3$ (A),
\item $C_1$, $C_3$ et $C_e$ (B),
\item $C_3$, $C_2$ et $C_e$ (C),
\item $C_2$, $C_1$ et $C_e$ (D).
\end{itemize}

Notons que le cercle de courbure n�gative ($C_e$) est toujours le
dernier dans les appels � [recurse].  Le fait que nous n'ayons pas
trait� de cercle ``de gauche'' au paragraphe pr�c�dent est maintenant
justifi� : un espace � gauche est aussi un espace � droite pour une
autre combinaison de cercles.


\begin{figureh}
  \centering
  \psset{unit=5mm}
  \begin{pspicture}(-4,-5.5)(9,5)
    \pscircle(0,0){3}      \put(-2,1.5){$C_1$}
    \pscircle(5.5,0){2.5}  \put(6,0){$C_2$}
    \pscircle(3.182,-3.857){2} \put(1.5,-4){$C_3$}
    \psset{linewidth=0.03mm}
    \pscircle(2.497,-0.395){-5.528}\put(7,4){$C_e$}
  %  \pscircle(3.03,-1.484){0.378} \put(1.2,-1.2){$C_i$}
    \put(2.7,-1.55){A}
    \put(-1,-4){B}
    \put(5.5,-4){C}
    \put(2,3.5){D}
  \end{pspicture}
  \caption{Construction compl�te � partir de trois cercles}
  \label{fig:start}
\end{figureh}

*)

let start c_1 c_2 c_3 min =
  let c_e = out_circle c_1 c_2 c_3 in
  let (ze,ce) = c_e in
  if ce>0. then failwith "Giving up!"
  else
    c_e::c_1::c_2::c_3::
    recurse c_1 c_2 c_3 min @
    recurse c_1 c_3 c_e min @
    recurse c_3 c_2 c_e min @
    recurse c_2 c_1 c_e min


(*s

Pour commencer la construction, il faut tout d'abord disposer de trois
cercles tangents deux � deux, tous � courbure positive.  Voici comment
construire trois tels cercles $C_1$, $C_2$ et $C_3$, de rayons
quelconques $r_1$, $r_2$ et $r_3$.

Dans ce paragraphe nous raisonnons en rayons, les courbures ne nous
�tant d'aucun int�r�t.  Centrons $C_1$ en $O$, on a donc
$C_1=(O,r_1)$. Nous pouvons centrer $C_2$ en $A(r_1+r_2,0$), $C_2$
sera bien tangent � $C_1$.  Reste � construire un cercle $C_3$ de
rayon $r_3$, tangent � $C_1$ et $C_2$.  Par d�finition, son centre $B$
sera � la distance $r_1+r_3$ de $O$ et $r_2+r_3$ de $A$.  Il y a deux
positions possibles pour ce point $B$, qui sont les deux intersections
des cercles $(O,r_1+r_3)$ et $(B,r_2+r_3)$.

\begin{figureh}
  \centering
  \psset{unit=7mm}
  \begin{pspicture}(-2,-5)(7,5)
    {\psset{labels=none,ticks=none}\psaxes(0,0)(-5,-5)(12,5)}
    \pscircle(0,0){3}\psdot(0,0)\put(0.5,0.5){O}\put(-2,1.5){$C_1$}
    \pscircle(5,0){2}\psdot(5,0)\put(5.5,0.5){A}\put(5,-1){$C_2$}
    {\psset{linestyle=dotted}
      \pscircle(0,0){4.5}
      \pscircle(5,0){3.5}}
    \pscircle(3.3,-3){1.5}\put(4,-5){$C_3$}
  \end{pspicture}
  \caption{Calcul du centre du troisi�me cercle}
  \label{fig:third}
\end{figureh}

Pour rester coh�rent avec ce que nous avons fait jusqu'� pr�sent, nous
choisirons pour $C_3$ la position ``� droite'' de $C_1$ et $C_2$,
c'est � dire avec une ordonn�e n�gative (figure \ref{fig:third}).

On peut calculer analytiquement les coordonn�es de ces points
d'intersection � partir des �quations des cercles dans le cas g�n�ral.
Cependant, vues les positions choisies pour les centres, notre cas
particulier est bien plus simple.  Nous laisserons au lecteur le
plaisir de v�rifier le d�tail du calcul effectu� par la fonction
[third_center], qui calcule donc la position du cercle $C_3$.  Dans le
cas o� la valeur de [delta] serait n�gative suite aux erreurs
d'arrondis nous levons une exception pour le signaler.

%ZZZ failwith ?

*)

let third_center r1 r2 r3 =
  let ra=r1+.r3
  and rb=r2+.r3
  and x2=r1+.r2 in
  let ra2 = ra**2.
  in
  let x = (-. (x2**2.) -. ra2 +. rb**2.) /. (2. *.(-.x2)) in
  let delta = -. 4. *. (x**2. -. ra2)
  in
  if delta < 0. then failwith "third_center"
  else { Complex.re=x; Complex.im= -. (sqrt delta)/.2. }


(*s

Nous pouvons maintenant, � partir des rayons des trois cercles
initiaux et du rayon minimal, construire la liste des cercles de
l'empilement.  Il suffit pour cela de construire les trois
cercles initiaux puis d'appeler la fonction [start].

*)

let compute_circles r1 r2 r3 min =
  let c1 = { Complex.re=0.; Complex.im=0. }, 1./.r1
  and c2 = { Complex.re=r1+.r2; Complex.im=0.}, 1./.r2
  and c3 = third_center r1 r2 r3, 1./.r3
  in
  start c1 c2 c3 min


(*s

Tant que nous y sommes, nous pouvons dessiner un empilement de cercles
\emph{fractal}.  Pour cela nous calculons notre liste de cercles, puis
remplisson chacun de ces cercles avec une version r�duite de
l'empilement.  En fait ce que nous obtenons ici n'est pas vraiment
fractal, car nous ne reproduisons la figure qu'une fois dans chaque
cercle initial (figure \ref{fig:almostf}).  Le premier cercle de la
liste est le cercle ext�rieur de la figure, nous l'utilisons pour
calculer le facteur de r�duction � appliquer � la liste pour chacun
des cercles.

\begin{figureh}
  \centering
  \img{9cm}{exempleaf}
  \caption{Version presque fractale}
  \label{fig:almostf}
\end{figureh}

*)

let almost_fractal_circles ((ze,ce)::circles) cmax =
  List.flatten
    (List.map
       (fun (zi,ci) -> 
	 let scl = abs_float (ci /. ce)
	 in
	 List.map
	   (fun (z,c) -> Complex.add (cdiv (Complex.sub z ze) scl) zi, c*.scl)
	   (List.filter (fun (z,c) -> c*.scl<cmax) circles))
       circles)

(*s

Pour obtenir une vraie figure fractale, nous devons reproduire la
figure \emph{aussi} dans les nouveaux cercles que nous ajoutons, tant
que leur courbure est inf�rieure au maximum fix�.  �a fait pas mal
travailler l'ordinateur (il y a beaucoup plus de cercles), et le
r�sultat n'est pas tr�s joli.  Pour all�ger la figure
\ref{fig:fractal} nous avons choisi un rayon limite plus �lev� que
dans les exemples pr�c�dents.

\begin{figureh}
  \centering
  % 100 75 50 4
  \img{9cm}{exemplef}
  \caption{Version fractale}
  \label{fig:fractal}
\end{figureh}

*)


let fractal_circles ((ze,ce)::circles) cmax =
  let smallest = List.fold_left (fun sml (z,c) -> min sml c) max_float circles
  in
  let rec help acc candidates =
    match candidates with
    | [] -> acc
    | (z,c)::others -> 
	if (c/.ce)*.smallest > cmax then
	  (* this candidate has too big a curve, skip it *)
	  help acc others
	else
	  let newcircles = 
	    let scl = abs_float (c /. ce) in
	    List.map
	      (fun (zi,ci) -> Complex.add (cdiv (Complex.sub zi ze) scl) z, ci*.scl)
	      (List.filter (fun (z,c) -> c*.scl<cmax) circles)
	  in
	  (* new circles are added to [acc] but are also new [candidates] *)
          (* to receive a copy of the original figure *)
	  help (acc @ newcircles) (others @ newcircles)
  in
  help [] circles

(*i
let fractal_circles ((ze,ce)::circles) cmax =
  let smallest = List.fold_left (fun sml (z,c) -> max sml c) 0. circles
  in
  let rec help acc candidates =
    match candidates with
    | [] -> acc
    | (z,c)::others -> 
	if (c/.ce)*.smallest > cmax then
	  (* this candidate has too big a curve, skip it *)
	  help acc others
	else
	  let newcircles = 
	    let scl = abs_float (c /. ce) in
	    List.map
	      (fun (zi,ci) -> Complex.add (cdiv (Complex.sub zi ze) scl) z, ci*.scl)
	      circles
	  in
	  (* new circles are added to [acc] but are also new [candidates] *)
          (* to receive a copy of the original figure *)
	  help (acc @ newcircles) (others @ newcircles)
  in

  help [] circles
i*)

(*s

Il nous reste � pr�senter l'information calcul�e, c'est � dire
convertir notre liste de cercles cod�s sous la forme d'un couple
(complexe, r�el) en une repr�sentation pour l'utilisateur.  Nous
laisserons le choix entre quatre possibilit�s :

\begin{enumerate}
\item un affichage graphique direct (pour voir imm�diatement l'aspect
  de l'empilement calcul�),
\item une liste textuelle des position, rayon et courbure des cercles
  (donn�es qui pourront �tre stock�es dans un fichier pour �tre
  ensuite utilis�es par un autre programme),
\item une repr�sentation graphique dans un fichier PostScript
  encapsul�,
\item idem mais avec un texte mis � l'�chelle dans chaque cercle.
\end{enumerate}

Pour les trois derniers cas, le r�sultat sera stock� dans un fichier
(la sortie standard par d�faut).

*)


(*s

Il n'y a pas grand chose � dire sur la fonction [to_text] qui
g�n�re la repr�sentation textuelle sur la sortie standard.

*)

let to_text fout circles r1 r2 r3 min =
  Printf.fprintf fout "# Empilement de cercles r1=%f r2=%f r3=%f min=%f\n" 
    r1 r2 r3 min;
  List.iter
    (fun ({Complex.re=x; Complex.im=y}, c) 
      -> Printf.fprintf fout "%f %f %f %f\n" x y (1./.c) c)
    circles

(*s

Pour les deux premi�res repr�sentations graphiques, nous utiliserons
une structure de donn�es graphiques fournie par le module Pictures. Il
permet de manipuler des figures compos�es d'une liste d'objets
graphiques �l�mentaires et fournit des fonctions permettant d'afficher
directement la figure � l'�cran ou de g�n�rer un fichier postscript
encapsul�.  Le seul type d'objets dont nous aurons besoin est
[Circle(x,y,r)] o� [x] et [y] sont les coordonn�es du centre du cercle
et [r] son rayon (tous trois de type [float]).  La fonction
[make_picture] retourne une valeur de type [pict].

*)

let make_pict circles =

  let pict_of_vc ({Complex.re=x;Complex.im=y}, c) =
    [ Picture.Circle(x,y,abs_float(1./.c)) ](*i;
      CText(x,y,string_of_int (int_of_float (c*.100.))) ] i*)

  in
  Picture.make_picture (List.flatten (List.map pict_of_vc circles))
;;


(*s

Le dessin illustrant l'article \cite{empilements_de_cercles} contient
dans chaque cercle un nombre repr�sentant sa courbure.  Ce nombre est
centr� et d'une taille remplissant son cercle.  Ceci n'est pas
possible avec le module Pictures, mais peut facilement �tre r�alis� en
PostScript.  Nous g�n�rons donc directement du code PostScript
\cite{postscript:blue} que nous ne commenterons pas ici.  La fonction
[to_eps] g�n�re le fichier sur la sortie standard. Le r�sultat est une
figure du type de la figure \ref{fig:exnb}.  Le nombre affich� dans
chaque cercle est la partie enti�re de la courbure multipli�e par le
param�tre [sc]. [lw] fixera l'�paisseur des traits.

\begin{figureh}
  \centering
    % -E -s 1000  150 75 50 5
  \img{9cm}{exemplenb}
  \caption{Empilement avec indication des courbures}
  \label{fig:exnb}
\end{figureh}

*)
let version = 0.96

let to_eps fout (c0::circles) r1 r2 r3 min sc lw =
  Printf.fprintf fout "%%!PS-Adobe-2.0 EPSF-2.0
%%%%Title: Empilement de cercles r1=%f r2=%f r3=%f min=%f
%%%%Creator: circles.ml %f\n" 
    r1 r2 r3 min version;
  let x,y,r = match c0 with ({Complex.re=x;Complex.im=y},c) -> x,y,-1./.c in
  Printf.fprintf fout "%%%%BoundingBox: %f %f %f %f\n" (x-.r) (y-.r) (x+.r) (y+.r);
  Printf.fprintf fout "%s"
"%%DocumentFonts: Helvetica-Bold
%%EndComments

/textincircle { % x y r t
    /t exch def
    /r exch def
    /y exch def
    /x exch def

    gsave

    % compute text size
    newpath 0 0 moveto
    t true charpath flattenpath pathbbox
    2 index sub /height exch def
    2 index sub /width exch def
    pop pop
    
    % gs BUG? pathbbox gives wrong width for strings with odd numbers of chars
    t stringwidth pop /width exch def
    
    % draw circle
    x y translate
    newpath 0 0 r 0 360 arc stroke

    % set text scale
    height width atan cos r mul 2 mul width div dup scale

    % draw centered text
    width 2 div neg height 2 div neg moveto t show

    grestore
} def


/c { % x y r c

    (mystring) cvs textincircle

} def
%%EndProlog

/Helvetica-Bold findfont 1 scalefont setfont
";

  Printf.fprintf fout "%f setlinewidth\n" lw;
  Printf.fprintf fout "newpath %f %f %f 0 360 arc stroke\n" x y r;
  List.iter
    (fun ({Complex.re=x; Complex.im=y}, c) 
      -> Printf.fprintf fout "%f %f %f %i c\n"
	  x y (1./.c) (int_of_float (c *. sc)))
    circles;
  Printf.fprintf fout "%s" "%%Trailer\n"


(*s

Pour finir, il nous faut traiter les arguments du programme, lancer
les calculs et sauver ou afficher les r�sultats.  Le programme attend
comme arguments quatre nombres d�cimaux (les rayons des trois cercles
initiaux et le rayon minimal) et accepte les options \texttt{-t} pour
g�n�rer la repr�sentation texte, \texttt{-e} pour g�n�rer la
repr�sentation postscript encapsul� et \texttt{-E} pour celle avec les
nombres dans les cercles. Si aucune de ces trois options n'est donn�e,
l'empilement de cercles sera affich� � l'�cran.  La c�t� de la fen�tre
peut alors �tre sp�cifi�e par l'option \texttt{-w}.  Les options
\texttt{-f} et \texttt{-F} s�lectionnent les versions fractales.
Enfin \texttt{-s} et \texttt{-l} fixent, en conjonction avec
\texttt{-E}, le facteur multiplicateur appliqu� � la courbure pour
d�terminer le nombre affich� dans chaque cercle et l'�paisseur des
traits.  Le nom du fichier de sortie (par d�faut la sortie standard)
sera fix� par \texttt{-o}.

*)

let text    = ref false
and eps     = ref false
and eps_nb  = ref false
and output  = ref ""

and fractal = ref false
and almostf = ref false
and wsize   = ref 500
and scale   = ref 1000.
and lw      = ref 1.

let all_rads = ref false
and rads = Array.make 4 0.

let get_rads = 
  let cpt = ref 0
  in
  fun s -> rads.(!cpt) <- float_of_string s;
    incr cpt; if !cpt=4 then all_rads:=true
	
let spec = [
  ("-t", Arg.Set text,         "text representation");
  ("-e", Arg.Set eps,          "eps file");
  ("-E", Arg.Set eps_nb,       "eps file with numbers");
  ("-o", Arg.Set_string output,"output file (default stdout)");
  ("-w", Arg.Set_int wsize,    "<size> window size for on screen display " ^ 
   "(default " ^ string_of_int !wsize ^ ")");
  ("-s", Arg.Set_float scale,  "<scale> for eps, multiply label values " ^
   "(default " ^ string_of_float !scale ^ ")");
  ("-l", Arg.Set_float lw,     "<linewidth> for eps " ^ 
   "(default " ^ string_of_float !lw ^ ")");
  ("-f", Arg.Set almostf,      "almost fractal figure");
  ("-F", Arg.Set fractal,      "fractal figure")
]
and usage_msg = "circles r1 r2 r3 rmin (draw on screen by default)"
;;
	
let main r1 r2 r3 min =
  (* compute *)
  let cl = 
    let circles = compute_circles r1 r2 r3 min in 
    circles @ (if !fractal then fractal_circles circles (1./.min) else
    if !almostf then almost_fractal_circles circles (1./.min) else [])
  in
  Printf.fprintf stderr "Computed %i circles.\n" (List.length cl);
  flush stdout;

  let fout = if !output="" then stdout else open_out !output
  in

  (*c display *)
  if !text then        to_text fout cl r1 r2 r3 min               (* text *)  
  else if !eps then    Picture.to_eps fout (make_pict cl)         (* eps *)
  else if !eps_nb then to_eps fout cl r1 r2 r3 min !scale !lw     (* eps with nbs *)
  else begin
    let ws = string_of_int !wsize in                         (* screen *)
    Graphics.open_graph (":0 " ^ ws ^ "x" ^ ws);
    Picture.to_screen (make_pict cl) 0 0 !wsize !wsize;
    Graphics.read_key (); ()
  end;
  if fout <> stdout then close_out fout;

  in
  Arg.parse spec get_rads usage_msg;
  if !all_rads then main rads.(0) rads.(1) rads.(2) rads.(3)
  else Arg.usage spec usage_msg

(*s

Ce programme a �galement �t� traduit en PostScript (voir fichier
\texttt{circles.eps}).  Les param�tres (tailles des trois cercles de
d�part et taille minimale) sont � �diter directement dans le fichier
qui peut �tre ouvert avec un logiciel de visualisation de documents
PostScript.

La traduction n'est pas difficile (pour quelqu'un familier avec
PostScript) mais l'�criture directe aurait �t� certainement plus
d�licate.  En effet, PostScript ne fournit pas de mani�re directe la
r�cursion ni les structures de donn�es.  Il n'est pas bien difficile
de les mettre en \oe uvre, mais cela constitue une distraction du but
initial pour le programmeur.  Un point plus fondamental est que le
programmeur doit alors formuler une solution en terme de concepts qui
ne sont pas directement fournis par le langage, c'est � dire qu'il a
un langage pour penser la solution et un langage pour la mettre en \oe
uvre.  Il est bien plus confortable de pouvoir penser directement une
solution dans le langage dans lequel elle sera mise en \oe uvre.

*)

(*

\bibliographystyle{fplain}
\bibliography{biblio}

*)
