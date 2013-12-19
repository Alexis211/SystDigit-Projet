<TeXmacs|1.0.7.19>

<style|generic>

<\body>
  <doc-data|<doc-title|Sp�cification processeur>>

  Nous proposons ici une sp�cification pour un processeur minimaliste 16 bit
  RISC.

  <section|Registres>

  La machine dispose de 8 registres \S g�n�raux \T :

  \;

  <\itemize>
    <item>0. Registre <verbatim|Z> ou z�ro (valant tout le temps 0...)

    <item>1. Registre <verbatim|A>

    <item>2. Registre <verbatim|B>

    <item>3. Registre <verbatim|C>

    <item>4. Registre <verbatim|D>

    <item>5. Registre <verbatim|E>, �crit par certaines instructions
    (multiplication, division...) et utilis� pr�f�rentiellement comme
    registre temporaire pour certaines instructions compos�es

    <item>6. Registre <verbatim|F>, ou <verbatim|RA> (return adresse), �crit
    par l'instruction <verbatim|jal>

    <item>7. Registre <verbatim|G>, ou <verbatim|SP> (stack pointer), utilis�
    par les instructions <verbatim|pop> et <verbatim|push>
  </itemize>

  De plus, le processeur dispose d'un registre non manipulable, le registre
  <verbatim|PC> (program counter).

  Les num�ros de registres sont donc cod�s sur 3 bits.

  <section|M�moire>

  La m�moire est adress�e sur 16 bits, il y a donc 64ko disponnibles.

  Diff�rentes zones de m�moire sont � d�finir (ROM, RAM, MMIO)

  <section|Jeu d'instruction>

  Les instructions sont cod�es sur 16 bits.

  <subsection|Types d'instructions>

  <paragraph|Format de base><tabular*|<tformat|<cwith|2|2|1|-1|cell-lborder|1px>|<cwith|2|2|1|-1|cell-bborder|1px>|<cwith|2|2|1|-1|cell-tborder|1px>|<cwith|2|2|3|3|cell-rborder|1px>|<table|<row|<cell|5
  bits>|<cell|3 bits>|<cell|8 bits>>|<row|<cell|<math|I>>|<cell|<math|R>>|<cell|<math|\<ldots\>>>>>>>

  <paragraph|Format <math|R>><tabular*|<tformat|<cwith|2|2|1|-1|cell-lborder|1px>|<cwith|2|2|1|-1|cell-bborder|1px>|<cwith|2|2|1|-1|cell-tborder|1px>|<cwith|2|2|3|3|cell-rborder|1px>|<cwith|2|2|5|5|cell-rborder|1px>|<table|<row|<cell|5
  bits>|<cell|3 bits>|<cell|3 bits>|<cell|3 bits>|<cell|2
  bits>>|<row|<cell|<math|I>>|<cell|<math|R>>|<cell|<math|R<rsub|A>>>|<cell|<math|R<rsub|B>>>|<cell|<math|f>>>>>>

  <paragraph|Format <math|I>><tabular*|<tformat|<cwith|2|2|1|-1|cell-lborder|1px>|<cwith|2|2|1|-1|cell-bborder|1px>|<cwith|2|2|1|-1|cell-tborder|1px>|<cwith|2|2|3|3|cell-rborder|1px>|<table|<row|<cell|5
  bits>|<cell|3 bits>|<cell|8 bits>>|<row|<cell|<math|I>>|<cell|<math|R>>|<cell|<math|d>>>>>>

  <paragraph|Format K><tabular*|<tformat|<cwith|2|2|1|-1|cell-lborder|1px>|<cwith|2|2|1|-1|cell-bborder|1px>|<cwith|2|2|1|-1|cell-tborder|1px>|<cwith|2|2|3|3|cell-rborder|1px>|<cwith|2|2|4|4|cell-rborder|1px>|<table|<row|<cell|5
  bits>|<cell|3 bits>|<cell|3 bits>|<cell|5
  bits>>|<row|<cell|<math|I>>|<cell|<math|R>>|<cell|<math|R<rprime|'>>>|<cell|<math|d>>>>>>

  <paragraph|Format <math|J>><tabular|<tformat|<cwith|2|2|1|2|cell-lborder|1px>|<cwith|2|2|1|2|cell-bborder|1px>|<cwith|2|2|1|2|cell-tborder|1px>|<cwith|2|2|2|2|cell-rborder|1px>|<cwith|2|2|1|-1|cell-halign|c>|<table|<row|<cell|5
  bits>|<cell|11 bits>>|<row|<cell|<math|I>>|<cell|<math|d>>>>>>

  <section|Tableau d'instructions>

  Certain noms d'instuctions sont en <em|italique>, il s'agit de signifier
  qu'il s'agit d'un alias (optionnel) pour une autre instruction.

  <big-table|<tabular*|<tformat|<cwith|1|1|1|-1|cell-bborder|1px>|<table|<row|<cell|<strong|<math|I>>>|<cell|<strong|format>>|<cell|<strong|<math|f>>>|<cell|<strong|description>>|<cell|<strong|action>>|<cell|<strong|valeurs
  sign�s ?>>>|<row|<cell|00000>|<cell|R>|<cell|0>|<cell|add>|<cell|<math|R\<leftarrow\>R<rsub|A>+R<rsub|B>>>|<cell|sign�>>|<row|<cell|>|<cell|>|<cell|1>|<cell|sub>|<cell|<math|R\<leftarrow\>R<rsub|A>-R<rsub|B>>>|<cell|sign�>>|<row|<cell|>|<cell|>|<cell|2>|<cell|mul>|<cell|<math|R\<leftarrow\>lo<around*|(|R<rsub|A>\<times\>R<rsub|B>|)>>>|<cell|sign�>>|<row|<cell|>|<cell|>|<cell|>|<cell|>|<cell|<math|E\<leftarrow\>hi<around*|(|R<rsub|A>\<times\>R<rsub|B>|)>>
  si <math|E\<neq\>R>>|<cell|>>|<row|<cell|>|<cell|>|<cell|3>|<cell|div>|<cell|<math|R\<leftarrow\>q<around*|(|R<rsub|A>,R<rsub|B>|)>>>|<cell|sign�>>|<row|<cell|>|<cell|>|<cell|>|<cell|>|<cell|<math|E\<leftarrow\>r<around*|(|R<rsub|A>,R<rsub|B>|)>>
  si <math|E\<neq\>R>>|<cell|>>|<row|<cell|00001>|<cell|R>|<cell|0>|<cell|addu>|<cell|idem
  add>|<cell|non sign�>>|<row|<cell|>|<cell|>|<cell|1>|<cell|subu>|<cell|idem
  sub>|<cell|non sign�>>|<row|<cell|>|<cell|>|<cell|2>|<cell|mulu>|<cell|idem
  mul>|<cell|non sign�>>|<row|<cell|>|<cell|>|<cell|3>|<cell|divu>|<cell|idem
  div>|<cell|non sign�>>|<row|<cell|00010>|<cell|R>|<cell|0>|<cell|or>|<cell|<math|R\<leftarrow\><around*|(|R<rsub|A>\<vee\>R<rsub|B>|)>>>|<cell|>>|<row|<cell|>|<cell|>|<cell|1>|<cell|and>|<cell|<math|R\<leftarrow\><around*|(|R<rsub|A>\<wedge\>R<rsub|B>|)>>>|<cell|>>|<row|<cell|>|<cell|>|<cell|2>|<cell|xor>|<cell|<math|R\<leftarrow\><around*|(|R<rsub|A>\<oplus\>R<rsub|B>|)>>>|<cell|>>|<row|<cell|>|<cell|>|<cell|3>|<cell|nor>|<cell|<math|R\<leftarrow\>not
  <around*|(|R<rsub|A>\<vee\>R<rsub|B>|)>>>|<cell|>>|<row|<cell|00011>|<cell|R>|<cell|0>|<cell|lsl>|<cell|<math|R\<leftarrow\><around*|(|R<rsub|A>\<ll\>R<rsub|B>|)>>>|<cell|>>|<row|<cell|>|<cell|>|<cell|1>|<cell|<em|lsl>>|<cell|>|<cell|>>|<row|<cell|>|<cell|>|<cell|2>|<cell|lsr>|<cell|<math|R\<leftarrow\><around*|(|R<rsub|A>\<gg\>R<rsub|B>|)>>
  (logical)>|<cell|(non sign�)>>|<row|<cell|>|<cell|>|<cell|3>|<cell|asr>|<cell|<math|R\<leftarrow\><around*|(|R<rsub|A>\<gg\>R<rsub|B>|)>>
  (arith)>|<cell|(sign�)>>|<row|<cell|00100>|<cell|R>|<cell|0>|<cell|<em|se>>|<cell|>|<cell|>>|<row|<cell|>|<cell|>|<cell|1>|<cell|<em|sne>>|<cell|>|<cell|>>|<row|<cell|>|<cell|>|<cell|2>|<cell|se>|<cell|<math|R\<leftarrow\><around*|(|R<rsub|A>=R<rsub|B>?1:0|)>>>|<cell|>>|<row|<cell|>|<cell|>|<cell|3>|<cell|sne>|<cell|<math|R\<leftarrow\><around*|(|R<rsub|A>\<neq\>R<rsub|B>?1:0|)>>>|<cell|>>|<row|<cell|00101>|<cell|R>|<cell|0>|<cell|slt>|<cell|<math|R<rsub|>\<leftarrow\><around*|(|R<rsub|A>\<less\>R<rsub|B>?1:0|)>>>|<cell|sign�>>|<row|<cell|>|<cell|>|<cell|1>|<cell|sle>|<cell|<math|R\<leftarrow\><around*|(|R<rsub|A>\<leqslant\>R<rsub|B>?1:0|)>>>|<cell|sign�>>|<row|<cell|>|<cell|>|<cell|2>|<cell|sltu>|<cell|<math|R<rsub|>\<leftarrow\><around*|(|R<rsub|A>\<less\>R<rsub|B>?1:0|)>>>|<cell|non
  sign�>>|<row|<cell|>|<cell|>|<cell|3>|<cell|sleu>|<cell|<math|R\<leftarrow\><around*|(|R<rsub|A>\<leqslant\>R<rsub|B>?1:0|)>>>|<cell|non
  sign�>>|<row|<cell|00110>|<cell|I>|<cell|>|<cell|incri>|<cell|<math|R\<leftarrow\><around*|(|R+d|)>>>|<cell|<math|d>
  sign�>>|<row|<cell|00111>|<cell|I>|<cell|>|<cell|shi>|<cell|<math|R\<leftarrow\><around*|(|R\<ll\>d|)>>>|<cell|<math|d>
  sign�>>|<row|<cell|01000>|<cell|J>|<cell|>|<cell|j>|<cell|<math|PC\<leftarrow\>PC+d>>|<cell|>>|<row|<cell|01001>|<cell|J>|<cell|>|<cell|jal>|<cell|<math|F\<leftarrow\><around*|(|PC+2|)>
  ; PC\<leftarrow\>PC+d>>|<cell|>>|<row|<cell|01010>|<cell|R<math|>>|<cell|0>|<cell|jr>|<cell|<math|PC\<leftarrow\>R>>|<cell|>>|<row|<cell|>|<cell|<math|>>|<cell|1>|<cell|jalr>|<cell|<math|><math|F\<leftarrow\><around*|(|PC+2|)>
  ; PC\<leftarrow\>R>>|<cell|>>|<row|<cell|>|<cell|>|<cell|2>|<cell|jer>|<cell|if
  <math|R<rsub|A>=R<rsub|B>> then <math|PC\<leftarrow\>R<rsub|>>>|<cell|>>|<row|<cell|>|<cell|>|<cell|3>|<cell|jner>|<cell|if
  <math|R<rsub|A>\<neq\>R<rsub|B>> then <math|PC\<leftarrow\>R<rsub|>>>|<cell|>>|<row|<cell|01011>|<cell|R>|<cell|0>|<cell|jltr>|<cell|if
  <math|R<rsub|A>\<less\>R<rsub|B>> then <math|PC\<leftarrow\>R<rsub|>>>|<cell|sign�>>|<row|<cell|>|<cell|>|<cell|1>|<cell|jler>|<cell|if
  <math|R<rsub|A>\<leqslant\>R<rsub|B>> then
  <math|PC\<leftarrow\>R<rsub|>>>|<cell|sign�>>|<row|<cell|>|<cell|>|<cell|2>|<cell|jltru>|<cell|if
  <math|R<rsub|A>\<less\>R<rsub|B>> then <math|PC\<leftarrow\>R<rsub|>>>|<cell|non
  sign�>>|<row|<cell|>|<cell|>|<cell|3>|<cell|jleru>|<cell|if
  <math|R<rsub|A>\<leqslant\>R<rsub|B>> then
  <math|PC\<leftarrow\>R<rsub|>>>|<cell|non
  sign�>>|<row|<cell|01100>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>>|<row|<cell|01101>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>>|<row|<cell|01110>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>>|<row|<cell|01111>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>>|<row|<cell|10000>|<cell|K>|<cell|>|<cell|lw>|<cell|<math|R\<leftarrow\>mem<around*|(|R<rprime|'>+d|)>>
  (16 bits)>|<cell|>>|<row|<cell|10001>|<cell|K>|<cell|>|<cell|sw>|<cell|<math|mem<around*|(|R<rprime|'>+d|)>\<leftarrow\>R>
  (16 bits)>|<cell|>>|<row|<cell|10010>|<cell|K>|<cell|>|<cell|lb>|<cell|<math|R\<leftarrow\>mem<around*|(|R<rprime|'>+d|)>>
  (8 bits)>|<cell|>>|<row|<cell|10011>|<cell|K>|<cell|>|<cell|sb>|<cell|<math|mem<around*|(|R<rprime|'>+d|)>\<leftarrow\>R>
  (8 bits)>|<cell|>>|<row|<cell|10100>|<cell|R>|<cell|*>|<cell|lwr>|<cell|<math|R\<leftarrow\>mem<around*|(|R<rsub|A>+R<rsub|B>|)>>
  (16 bits)>|<cell|>>|<row|<cell|10101>|<cell|R>|<cell|*>|<cell|swr>|<cell|<math|mem<around*|(|R<rsub|A>+R<rsub|B>|)>\<leftarrow\>R>
  (16 bits)>|<cell|>>|<row|<cell|10110>|<cell|R>|<cell|*>|<cell|lbr>|<cell|<math|R\<leftarrow\>mem<around*|(|R<rsub|A>+R<rsub|B>|)>>
  (8 bits)>|<cell|>>|<row|<cell|10111>|<cell|R>|<cell|*>|<cell|sbr>|<cell|<math|mem<around*|(|R<rsub|A>+R<rsub|B>|)>\<leftarrow\>R>
  (8 bits)>|<cell|>>|<row|<cell|11000>|<cell|I>|<cell|>|<cell|lil>|<cell|<math|R<rsub|lo>\<leftarrow\>d>>|<cell|>>|<row|<cell|11001>|<cell|I>|<cell|>|<cell|lilz>|<cell|<math|R<rsub|lo>\<leftarrow\>d
  ; R<rsub|hi>\<leftarrow\>0>>|<cell|>>|<row|<cell|11010>|<cell|I>|<cell|>|<cell|liu>|<cell|<math|R<rsub|hi>\<leftarrow\>d>>|<cell|>>|<row|<cell|11011>|<cell|I>|<cell|>|<cell|liuz>|<cell|<math|R<rsub|hi>\<leftarrow\>d
  ; R<rsub|lo>\<leftarrow\>0>>|<cell|>>|<row|<cell|11100>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>>|<row|<cell|11101>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>>|<row|<cell|11110>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>>|<row|<cell|11111>|<cell|>|<cell|>|<cell|<samp|>>|<cell|>|<cell|>>>>>|Instructions
  reconnues par le microproceseur>

  <big-table|<tabular|<tformat|<cwith|1|1|1|-1|cell-bborder|1px>|<table|<row|<cell|<strong|Nom>>|<cell|<strong|Action>>|<cell|<strong|Code
  assembleur de base �quivalent>>>|<row|<cell|push
  <math|R>>|<cell|<math|G\<leftarrow\>G-2 ;mem<around*|(|G|)>\<leftarrow\>R
  >>|<cell|incri <math|G,-2>>>|<row|<cell|>|<cell|>|<cell|sw
  <math|R\<nocomma\>>, <math|G>>>|<row|<cell|pop
  <math|R>>|<cell|<math|R\<leftarrow\>mem<around*|(|G|)> ;
  G\<leftarrow\>G+2>>|<cell|lw <math|R>, <math|G>>>|<row|<cell|>|<cell|>|<cell|incri
  <math|G>, 2>>|<row|<cell|move <math|R,R<rsub|A>>>|<cell|<math|R\<leftarrow\>R<rsub|A>>>|<cell|add
  <math|R>, <math|R<rsub|A>>, <math|Z>>>|<row|<cell|addi, subi,
  ...>|<cell|<math|R\<leftarrow\>R<rsub|A>+d>>|<cell|(utilise E comme
  registre temporaire)>>|<row|<cell|not <math|R,R<rsub|A>>>|<cell|<math|R\<leftarrow\>not
  R<rsub|A>>>|<cell|<math|nor R,R<rsub|A>,Z>>>|<row|<cell|jz <math|R>,
  addr>|<cell|if <math|R=0> then <math|PC\<leftarrow\>addr>>|<cell|lil E,
  lo(addr) ; liu E, hi(addr) OU lilz E, addr>>|<row|<cell|>|<cell|>|<cell|jer
  R, E, Z>>|<row|<cell|jnz <math|R>, addr>|<cell|if <math|R\<neq\>0> then
  <math|PC\<leftarrow\>addr>>|<cell|lil E, lo(addr) ; liu E, hi(addr) OU lilz
  E, addr>>|<row|<cell|>|<cell|>|<cell|jner R, E, Z>>>>>|Instructions
  suppl�mentaires (produites par l'assembleur)>\ 
</body>

<\initial>
  <\collection>
    <associate|language|french>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|1>>
    <associate|auto-10|<tuple|4|1>>
    <associate|auto-11|<tuple|1|2>>
    <associate|auto-12|<tuple|2|3>>
    <associate|auto-2|<tuple|2|1>>
    <associate|auto-3|<tuple|3|1>>
    <associate|auto-4|<tuple|3.1|1>>
    <associate|auto-5|<tuple|1|1>>
    <associate|auto-6|<tuple|2|1>>
    <associate|auto-7|<tuple|3|1>>
    <associate|auto-8|<tuple|4|1>>
    <associate|auto-9|<tuple|5|1>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|table>
      <tuple|normal|Instructions reconnues par le
      microproceseur|<pageref|auto-11>>

      <tuple|normal|Instructions suppl�mentaires (produites par
      l'assembleur)|<pageref|auto-12>>
    </associate>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|Registres>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|M�moire>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-2><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|Jeu
      d'instruction> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-3><vspace|0.5fn>

      <with|par-left|<quote|1.5fn>|Types d'instructions
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-4>>

      <with|par-left|<quote|6fn>|Format de base
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-5><vspace|0.15fn>>

      <with|par-left|<quote|6fn>|Format <with|mode|<quote|math>|R>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-6><vspace|0.15fn>>

      <with|par-left|<quote|6fn>|Format <with|mode|<quote|math>|I>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-7><vspace|0.15fn>>

      <with|par-left|<quote|6fn>|Format K
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-8><vspace|0.15fn>>

      <with|par-left|<quote|6fn>|Format <with|mode|<quote|math>|J>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-9><vspace|0.15fn>>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|Tableau
      d'instructions> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-10><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>