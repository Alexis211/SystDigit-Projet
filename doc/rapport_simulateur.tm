<TeXmacs|1.0.7.19>

<style|generic>

<\body>
  <doc-data|<doc-title|Rapport : simulateur de
  circuit>|<doc-author|<author-data|<author-name|Alex
  AUVOLAT>>>|<doc-date|2013-11-12 09h00>>

  Dans le cadre du cours <em|Système Digital : de l'algorithme au circuit> de
  <name|J.Vuillemin>, j'ai écrit un simulateur de circuit capable de faire
  fonctionner un circuit préalablement compilé dans le langage MiniJazz.

  Le simulateur que j'ai écrit est codé en C (essentiellement pour la
  rapidité d'exécution), et nécessite un pré-traîtement des données par un
  programme en Caml. Le simulateur en C ne lit pas directement les netlist
  produites par MiniJazz, mais un fichier <verbatim|.dumb> qui est produit
  par le pré-processeur.

  Contenu de l'archive ci-jointe :

  <\verbatim-code>
    README

    \ Diverses documentations. À lire.

    \;

    sched/

    \ Dans ce répertoire se trouve le pré-processeur, qui effectue les
    opérations suivantes :

    \ - lecture d'une netlist

    \ - tri topologique

    \ - diverses optimisations

    \ - écriture d'un fichier .net et d'un fichier .dumb contenant le
    résultat.

    Pour compiler le pré-processeur :

    $ cd sched/

    $ ocamlbuild main.byte

    \;

    csim/

    \ Dans ce répertoire se trouve le simulateur en C.

    Pour le compiler :

    $ cd csim/

    $ make

    Les options du simulateur sont décrites lors de l'invocation de celui-ci
    sans arguments.

    \;

    tests/

    \ Ce répertoire contient un certain nombre de fichiers de test au format
    minijazz. Le Makefile permet d'effectuer les appels au compilateur, au
    pré-processeur et au simulateur avec la syntaxe suivante :

    $ cd tests/

    $ make nadder.sim \ \ \ \ # nadder.mj -\<gtr\> nadder.net -\<gtr\>
    nadder.dumb -\<gtr\> csim
  </verbatim-code>

  Les points importants à souligner dans mon travail sont :

  <\itemize>
    <item>Définition d'un format de données intermédiaire pour les Netlist,
    permettant l'écriture d'un simulateur en C. Définition conjointe de ce
    format et de la structure du simulateur, afin d'écrire un simulateur le
    mieux optimisé possible.

    <item>Écriture d'un optimiseur de Netlist, qui est capable d'effectuer
    plusieurs simplifications, dont :

    <\itemize>
      <item>propagation des constantes ;

      <item>détection de variables au contenu identique ;

      <item>suppression de variables inutiles ;

      <item>suppression d'opérations arithmétiques inutiles.
    </itemize>

    L'application de ces passes d'optimisation réduit généralement la taille
    d'une Netlist d'environ 30%.
  </itemize>

  Les efforts d'optimisation ont été faits dans l'idée que le simulateur
  devra faire fonctionner un processeur MIPS à une fréquence raisonnable
  (idéalement, plusieurs centaines, voire milliers, de cycle par seconde). Le
  résultat de ce travail se voit sur les programmes <verbatim|clockHMS.mj> et
  <verbatim|clock2.mj> qui définissent deux horloges qui comptent modulo 60
  puis 60 puis 24 (donc qui comptent sur une journée, à un cycle par
  seconde). Avec mon simulateur optimisé, les deux horloges sont capables de
  simuler une journée entière de comptage, c'est-à-dire 86400 cycles, en 0.4
  secondes pour le premier et 0.9 secondes pour le second, sur un ordinateur
  moderne.
</body>

<\initial>
  <\collection>
    <associate|language|french>
  </collection>
</initial>