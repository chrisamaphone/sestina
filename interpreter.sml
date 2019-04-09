structure Interpreter =
struct

  datatype basetp = TBool | TRange of int * int | TString of string list

  datatype gentp = 
    TSum of (string * gentp) list 
  | TProd of (string * gentp) list
  | TBase of basetp 
  | TOne 
  (* | Rectp of ident * gentp | Var of ident *)

  type ident = int

  datatype exp = 
    Inj of string * exp 
  | Tuple of (string * exp) list
  | Case of exp * ((string * (ident list) * exp) list)
  | Var of ident
  | EUnit
  | ETrue
  | EFalse
  | Num of int
  | Str of string
  | EBool of bool

  val rando = Random.rand (19627, 9212987)

  fun randInRange min max = Random.randRange (min, max) rando

  fun randMember (l : 'a list) : 'a =
  let
    val idx = randInRange 0 (List.length l - 1)
  in
    List.nth (l, idx)
  end

  fun genbase (b : basetp) : exp =
    case b of
         TBool => randMember [EBool true, EBool false]
       | TRange (min, max) => Num (randInRange min max)
       | TString options => Str (randMember options)


  fun gen (tau : gentp) : exp =
    case tau of
         TOne => EUnit
       | TBase b => genbase b
       | TProd tps => Tuple (map (fn (s, t) => (s, gen t)) tps)
       | TSum tps =>
           let
             val (s, t) = randMember tps
           in
             Inj (s, gen t)
           end

  fun lookup kvs k =
    case kvs of
         [] => raise Match
       | ((k',v)::kvs) => if k=k' then v else lookup kvs k

  fun proj (elems : (string * exp) list) (field : string) =
    lookup elems field

  fun projStr elems field =
  let
    val Str s = lookup elems field
  in s end

  (* Tests *)

  val abcs = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
  "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]

  val monsterTypes = ["Zombie", "Vampire", "Ogre", "Dragon", "Orc", "Goblin",
  "Witch", "Grue", "Warlock", "Werewolf"]

  val TAlphabet = TBase (TString abcs)
  val TDigits = TBase (TRange (0, 9))
  val TMonsterType = TBase (TString monsterTypes)

  val StrongRange = TBase (TRange (5,9))
  val WeakRange = TBase (TRange (1,4))
  val TMonster = TProd [("Type", TMonsterType), 
    ("Stats", TSum [("Strong", StrongRange), ("Weak", WeakRange)])]


  (* Generate monster *)
  fun genMonster () = gen TMonster

  (* Generate a list of monsters *)
  fun dungeon n = List.tabulate (n, fn i => genMonster ())


  (* Tracery Stories *)
  val they = TBase (TString ["they"])
  val them = TBase (TString ["them"])
  val their = TBase (TString ["their"])
  val theirs = TBase (TString ["theirs"])
  val themself = TBase (TString ["themself"])

  val she = TBase (TString ["she"])
  val her = TBase (TString ["her"])

  val he = TBase (TString ["he"])
  val him = TBase (TString ["him"])
  val his = TBase (TString ["his"])

  val they_pronouns = TProd [("they", they), ("them", them), ("their", their)]
  val she_pronouns = TProd [("they", she), ("them", her), ("their", her)]
  val he_pronouns = TProd [("they", he), ("them", him), ("their", his)]

  val TPronouns = TSum [("they", they_pronouns), ("he", he_pronouns),
    ("she", she_pronouns)]


  fun genPronouns () =
  let
    val (Inj (_, Tuple heroPronouns)) = gen TPronouns
  in
    heroPronouns
  end

  fun genStory () = 
  let
    val heroPronouns = genPronouns ()
  in
    "Our hero went into the dungeons to find the treasure. "
    ^ (projStr heroPronouns "they") ^ " descended into the final gave, drew "
    ^ (projStr heroPronouns "their") ^ " sword, and fought the beast who faced "
    ^ (projStr heroPronouns "them") ^ "."
  end

  (* Later example ideas: 
  * - hearthstone cards 
  * - Tracery nested stories
  * - 2D grid mazes
  * - 2D tile game levels (pac man, sokoban?)
  * - nethack dungeons
  * *)


end
