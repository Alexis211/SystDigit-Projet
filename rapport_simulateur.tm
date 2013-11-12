<TeXmacs|1.0.7.19>

<style|generic>

<\body>
  <doc-data|<doc-title|Rapport : simulateur de
  circuit>|<doc-author|<author-data|<author-name|Alex
  AUVOLAT>>>|<doc-date|2013-11-12 09h00>>

  Dans le cadre du cours <em|Syst�me Digital : de l'algorithme au circuit> de
  <name|J.Vuillemin>, j'ai �crit un simulateur de circuit capable de faire
  fonctionner un circuit pr�alablement compil� dans le langage MiniJazz.

  Le simulateur que j'ai �crit est cod� en C (essentiellement pour la
  rapidit� d'ex�cution), et n�cessite un pr�-tra�tement des donn�es par un
  programme en Caml. Le simulateur en C ne lit pas directement les netlist
  produites par MiniJazz, mais un fichier <verbatim|.dumb> qui est produit
  par le pr�-processeur.

  Contenu de l'archive ci-jointe :

  <\verbatim-code>
    README

    \ Diverses documentations. � lire.

    \;

    sched/

    \ Dans ce r�pertoire se trouve le pr�-processeur, qui effectue les
    op�rations suivantes :

    \ - lecture d'une netlist

    \ - tri topologique

    \ - diverses optimisations

    \ - �criture d'un fichier .net et d'un fichier .dumb contenant le
    r�sultat.

    Pour compiler le pr�-processeur :

    $ cd sched/

    $ ocamlbuild main.byte

    \;

    csim/

    \ Dans ce r�pertoire se trouve le simulateur en C.

    Pour le compiler :

    $ cd csim/

    $ make

    Les options du simulateur sont d�crites lors de l'invocation de celui-ci
    sans arguments.

    \;

    tests/

    \ Ce r�pertoire contient un certain nombre de fichiers de test au format
    minijazz. Le Makefile permet d'effectuer les appels au compilateur, au
    pr�-processeur et au simulateur avec la syntaxe suivante :

    $ cd tests/

    $ make nadder.sim \ \ \ \ # nadder.mj -\<gtr\> nadder.net -\<gtr\>
    nadder.dumb -\<gtr\> csim
  </verbatim-code>

  Les points importants � souligner dans mon travail sont :

  <\itemize>
    <item>D�finition d'un format de donn�es interm�diaire pour les Netlist,
    permettant l'�criture d'un simulateur en C. D�finition conjointe de ce
    format et de la structure du simulateur, afin d'�crire un simulateur le
    mieux optimis� possible.

    <item>�criture d'un optimiseur de Netlist, qui est capable d'effectuer
    plusieurs simplifications, dont :

    <\itemize>
      <item>propagation des constantes ;

      <item>d�tection de variables au contenu identique ;

      <item>suppression de variables inutiles ;

      <item>suppression d'op�rations arithm�tiques inutiles.
    </itemize>

    L'application de ces passes d'optimisation r�duit g�n�ralement la taille
    d'une Netlist d'environ 30%.
  </itemize>

  Les efforts d'optimisation ont �t� faits dans l'id�e que le simulateur
  devra faire fonctionner un processeur MIPS � une fr�quence raisonnable
  (id�alement, plusieurs centaines, voire milliers, de cycle par seconde). Le
  r�sultat de ce travail se voit sur les programmes <verbatim|clockHMS.mj> et
  <verbatim|clock2.mj> qui d�finissent deux horloges qui comptent modulo 60
  puis 60 puis 24 (donc qui comptent sur une journ�e, � un cycle par
  seconde). Avec mon simulateur optimis�, les deux horloges sont capables de
  simuler une journ�e enti�re de comptage, c'est-�-dire 86400 cycles, en 0.4
  secondes pour le premier et 0.9 secondes pour le second, sur un ordinateur
  moderne.
</body>

<\initial>
  <\collection>
    <associate|language|french>
  </collection>
</initial>