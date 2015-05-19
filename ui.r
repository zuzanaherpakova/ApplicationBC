library(ggplot2)
library(shiny)
library(arules)
library(arulesViz)

shinyUI(pageWithSidebar(
  
  headerPanel("Aplikácia pre vizualizáciu medicínskych dát"),
  
    sidebarPanel(
      
      conditionalPanel(
      condition="input.tabs=='ÚVOD'",
       h4("Úvod")
        ),
      
      conditionalPanel(
        condition="input.tabs=='DÁTA'",
      #zvolim si ci chcem nacitat vlastne data alebo pracovat s uz nacitanymi a pripravenymi datami#
      h5("Vlož dáta:"),
      radioButtons("dataInput","", list("Načítaj vzorové dáta"=1,"Nahraj vlastné dáta"=2)),
      #ak si zvolim load sample data#
      conditionalPanel(
        condition="input.dataInput=='1'",
        h5("Vyber dáta:"),
      radioButtons("sampleData", "", 
      #moznost vyberu dat vhodnych pre fukncie zobrazenia barplotu a boxplotu#
      list("diabetic"=1,
           #tieto data su pripravene pre asociacne pravidla#
           "hepatic"=2)),
      wellPanel(
        h6(helpText("Vzorové dáta obsahujú údaje o pacientoch."))
        )),
      #ak si zvolim upload data#
      conditionalPanel(
        condition="input.dataInput=='2'",
         h5("Nahraj dáta:"),
      fileInput("file1","Vyber súbor:",
      accept=c('text/csv',
      'text/comma-separated-values,text/plain','.csv')),
    #zobrazenie hlavicky#
    wellPanel(
    h5("Uprav súbor s dátami"),
    h6("Súbor je s hlavičkou"),
    checkboxInput('header','Hlavička', TRUE),
    #separator#
    h6("Dáta sú oddelené:"),
    radioButtons('sep','',
                 c(Čiarkou=',',
                   Bodkočiarkou=';',
                   Tabulátorom='\t'),
                 ',')
    ),
    wellPanel(
    h6(helpText("Zaškrtni 'Hlavička' ak načítaný súbor obsahuje hlavičku")),
    h6(helpText("Zvoľ taký separátor, akým sú dáta v súbore navzájom oddelené"))
     )
    )
    ),
    
    conditionalPanel(
      condition="input.tabs=='O DÁTACH'",
      h5("Zo zvoleného súboru chcem zistiť:"),
      radioButtons("sumarVolba","", list("základné štatistické údaje"=1,"typ údajov"=2)),
  
    #conditionalPanel(
     #  condition="input.sumarVolba=='1'",
        wellPanel(
          h5(helpText("Základné štatistické údaje,")),
          h6(helpText("sú zobrazované pri numerických premmených zo súboru, inak je zobrazovaný iba počet pre jednotlivé skupiny údajov v stĺpci.")),
          h6(helpText("Viac informácií v záložke INFO"))
          ),
     #conditionalPanel(
        #condition="input.sumarVolba=='2",
       wellPanel(
          h5(helpText("Typ údajov,")),
          h6(helpText("alebo triedu pre jednotlivé stĺpce
                      zistím z označenia nachádzajúceho sa po dvojbodke.")),
          h6(helpText("Ak je za dvojbodkou označenie 'int' ide o numerickú premennú
                      v prípade označenia 'factor' ide o kategorickú premmenú(symbolickú).")),
          h6(helpText("Viac informácií v záložke INFO"))
       )
        ),
    
    conditionalPanel(
      condition="input.tabs=='GRAFY'",
      h5("Na zobrazenie použiť:"),
      radioButtons("plotvolba","", list("Boxplot"=1,"Stĺpcový graf"=2)),
       
      conditionalPanel(
        condition="input.plotvolba=='1'",
      h5("Vyber premennú"),
    selectInput("y", "Vyber prvý parameter:"," "),
    selectInput("x", "Vyber druhý parameter:"," "),
    h5("Orientácia krabicového grafu:"),
    radioButtons("horizontal","", 
                 list("horizontálna"=FALSE,
                      "vertikálna"=TRUE)),
    
   # tags$style(style="display:inline-block"),
    br(),
    actionButton("button1", "Čo je boxplot?")
    ),
    
    conditionalPanel(
      condition="input.plotvolba=='2'",
      h5("Vyber premennú"),
      selectInput("a", "Vyber prvý parameter:"," "),
      selectInput("b", "Vyber druhý parameter:"," "),
      br(),
      
      actionButton("button3", "Čo je stĺpcový graf?")
    )),
  
    conditionalPanel(
      condition="input.tabs=='ASOCIAČNÉ PRAVIDLÁ'",
      h5('Asociačne pravidlá'),
      numericInput("z", "Počet zobrazených asociačných pravidiel:", 3),
      numericInput("s", "Hranica minimálnej podpory:", 0.2, min=0, max=1, step=0.1),
      numericInput("c", "Hranica minimálnej spoľahlivosti:", 0.8, min=0, max=1, step=0.1),
    br(),
    br(),
    wellPanel(
      h6(helpText("Body v grafe zobrazujú konkrétne asociačné pravidlá nájdené podľa vstupných parametrov.")))
    ),
    
    conditionalPanel(
      condition="input.tabs=='INFO'",
      h4('Bližšie informácie'),
     (helpText('V tejte sekcii nájdete vysvetlenie pojmov 
                  použitých v aplikácii a taktiež najčastejšie otázky a odpovede spojené s použitím aplikácie.'))
    ),
    
    conditionalPanel(
     condition="input.tabs=='DOTAZNÍK'",
      h4('Dotazník')
      )
    ),
    
    mainPanel(
      tabsetPanel(type="tabs",  
                  
              tabPanel("ÚVOD", textOutput("uvod"),
                       br(),
                       p(h5('VITAJTE!'),
                         p('Táto aplikácia bola vytvorená ako praktický výstup bakalárskej práce.'),
                         p(HTML('Som študentkou tretieho ročníka odboru <a>Hosopodárska informatika</a>, 
                                Fakulta elektrotechniky a informatiky, Technická univerzita v Košiciach.')),
                         p('Cieľom mojej bakalárskej práce bolo vytvoriť aplikáciu pre podporu rozhodovania v medicínskej oblasti. 
                           Táto aplikácia je primárne určená pre lekárov, výskumných pracovníkov a  iný zdravotný personál, 
         ale aj pre používateľov pracujúci mimo tejto oblasti.
                           Aplikácia poskytuje možnosť načítania vlastných dát alebo použitia pripravených vzorových dát. 
                           Po načítaní dát aplikácia umožňuje vizualizáciu dát prostredníctvom grafov(boxplot a barplot) 
                           a dolovanie asociačných pravidiel v týchto načítaných medicínskych dátach.')
                         )
                       ),
              
              tabPanel("DÁTA",dataTableOutput("tabulka")
              ), 
              
              tabPanel("O DÁTACH",verbatimTextOutput("summary")
              ),
              
              tabPanel("GRAFY",plotOutput("graf1"),
                       downloadButton("down", "Stiahnúť graf"),
                       br(),
                       br(),
                       textOutput("textToPrint1"),
                       textOutput("textToPrint2"),
                       br(),
                       uiOutput("uiButton2")
              ),
              
              tabPanel("ASOCIAČNÉ PRAVIDLÁ",plotOutput("graf3"),
              verbatimTextOutput("pravidla")),
              
              tabPanel("INFO",textOutput('info'),
            h4('VYSVETLENIE POJMOV'),           
            h4('Základné štatistické ukazovatele'),
            HTML('Medzi základné štatistické ukazovatele, 
             ktoré ďalej používame na zobrazenie krabicového grafu sú:
           <a>minimum</a>, ktoré predstavuje najmenšiu hodnotu;
            <a>maximum</a>, ktoré predstavuje najväčšiu hodnotu z daného intervalu čísel;
            <a>medián</a> ako stredná hodnota čísel zoradených od najmenšieho po najväčší,
            <a>priemer</a> ako stredná hodnota vypočítaná zo súčtu všetkých hodnôt, ktorý je vydelený ich počtom a
              <a>kvantil</a> súboru, ktorý je vyjadrený ako hodnota k-tej časti, ak je súbor rozdelený na n rovnakých častí. 
                 Okrem mediánu sa často používajú <a>kvartily</a>, ktoré delia súbor na 4 časti'),
          
                       
          h4('Krabicový graf'), 
          HTML('V deskriptívnej štatistike sa <a>krabicový graf</a> alebo krabicový diagram
         používa ako jeden zo spôsobov grafickej vizualizácie dát pomocou ich kvartilov.
         Stredná časť diagramu(krabička) je zhora(pri vertikálnom zobrazení zprava) ohraničená 3.kvartilom, 
        zospodu(pri vertikálnom zobrazení zľava) 1.kvartilom
          a medzi nimi sa nachádza medián. 
          Ďalšie čiary nachádzajúce sa pod(vľavo) krabicou znázorňujú minimálnu hodnotu a čiara nad(vpravo) krabicou
        ukazuje na osi maximálnu hodnotu pre parameter, ktorý bol zvolený používateľom.'),
    
        
         h4('Stĺpcový graf'), 
          HTML('<a>Stĺpcový graf</a>(ang. barplot) alebo histogram je tvorený obdĺžnikmi, 
          ktorých základne (os "x") majú dĺžku zvolených intervalov, 
         a ktorých výšky (os "y") majú veľkosť príslušných absolútnych 
         alebo relatívnych početností zvolených tried.'),
         
         h4('Asociačné pravidlá'), 
          HTML ('Asociačné pravidlá sú výsledkom jednej z metód dolovania v dátach. 
            Daný algoritmus nájde z množiny údajov fekventované množiny a z nich následne odvodí asociačné pravidlá.
                Asociačné pravidlá sú aj nástrojom na tzv. analýzu nákupného košíka. V tomto zmysle sú vysvetlené aj nasledujúce pojmy.'),
          br(),
          HTML('<a> Asociačné pravidlo</a>  je definované ako implikácia X=> Y, pričom X a Y sú dve disjunktné podmnožiny univerzálnej množiny.'),
          br(),
          HTML('<a> Podpora</a>  je to, koľkokrát sa v databáze nachádzajú všetky položky v jedom košíku vydelené počtom všetkých košíkov.'),
          br(),
          HTML('<a> Spoľahlivosť</a>  je podiel, kde v čitateli je počet košíkov, ktoré keď obsahujú X, 
    tak do nich patrí aj Y a v menovateli je počet všetkých takých transakcií, ktoré obsahujú X.'),
         br(),
         
        ('___________________________________________________________________________________'),
         
         
       h4('OTÁZKY A ODPOVEDE'),
        h4('Ako zistím typ údajov?'),
         HTML('Typ údajov zistím v záložke <a>O DÁTACH</a>, kde si zvolím zistiť typ údajov.
          Ak je za dvojbodkou označenie "int" ide o numerickú premmennú,
           v prípade označenia "factor" je to symbolická premenná.'),
      br(),
        h4('Ako správne zvoliť premmenné pre zobrazenie grafu?'),
        ('Pre správne zobrazenie krabicového grafu je potrebné vybrať ako prvú 
        premennú údaj typu "int"(numerická premenná) a druhú premennú typu "factor" (symbolická premenná). 
          Táto aplikácia je navrhnutá tak,že používateľ si už vyberá iba z parametrov, ktoré sú vhodné pre zobrazenie.'),
      br(),
       h4('Ako rozumiem výsledku asociačných pravidiel?'),
       HTML('Asociačné pravidlo v tvare napr. {Unava=ano, Anorexia=ano} => {Nevolnost=ano} pre hranici podpory 0.3538462 a hranici spoľahlivosti 1 znamená,
            že z daných údajov o pacientoch algoritmus našiel pravidlo, že pacienti, ktorí boli unavení a majú anorexiu sa prejavuje nevoľnoť s 
            danou hranicou podpory a spoľahlivosti.'),
    br(),
    br(),
    br()
       

              
     ),
    tabPanel("DOTAZNÍK",textOutput('dotaznik'),
             br(),
           p('Ďakujem, že ste použili moju aplikáciu.'),
            HTML('Prosím venujte ešte chvíľu na vyplnenie nasledujúceho <i><a href="http://www.survio.com/survey/d/E7S3D7S6F5P2G6C3T"></i>dotazníka</a>.'),
            p ('Vaše odpovede mi pomôžu pri hodnotení aplikácie v mojej záverečnej práci.'),
           HTML('V prípade pripomienok ohľadom tejto aplikácie ma môžete kontaktovať aj na mojej e-mailovej adrese <i>zuzana.herpakova@student.tuke.sk')
      ),
      id="tabs"
      )
  ))
)


