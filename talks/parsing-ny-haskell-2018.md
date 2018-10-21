% Parsing parsed parseables, from music to Megaparsec
% Stuart Popejoy stuart@kadena.io @SirLensALot
% NY Haskell, Oct 2018

# Parsing in Haskell

* 16:32 **haskn00b**: any pointers on good regex libs in haskell? thanks!
* 16:33 **shw**: yeah ... parsec
* 16:33 **haskn00b**: cool!
* 16:53 **haskn00b**: shw: i don't see the regex part. How am I supposed to handle text?
* 16:54 **shw**: Regex can deceive you. Stretch out with your feelings!
* 16:54 **haskn00b**: _writes entire app in bash_

# Parser Combinators

## &nbsp;
```haskell
expr :: CharParsing m => m Expr
expr = (ENumber <$> scientific) <|>
       (EString <$> stringLiteral) <|>
       (EAtom <$> identifier) <|>
       (EList <$> parens (sepBy expr spaces))
```


## Main elements
- Alternative: `<|>`, `empty`, `some`, `many`
- Functor: `<$>`
- Applicative: `pure`, `<*>`, `<*`, `*>`
- MonadPlus: `mzero`, `mplus`
- Other: `sepBy`, `optional`, `satisfy`

## Dogfooding

```haskell
class (Alternative m,MonadPlus m) => CharParser m where
  satisfy :: (Char -> Bool) -> m Char

char :: CharParser m => Char -> m Char
char c = satisfy (c ==)

parens :: (CharParser m) => m a -> m a
parens p = char '(' *> p <* char ')'
```

## Parse you a LISP
```haskell
data Expr =
     ENumber Scientific
   | EString Text
   | EAtom Text
   | EList [Expr]

expr :: CharParsing m => m Expr
expr = (ENumber <$> scientific) <|>
       (EString <$> stringLiteral) <|>
       (EAtom <$> identifier) <|>
       (EList <$> parens (sepBy expr spaces))
```
```lisp
(defun div (a b)
  "Divide A by B and return rounded amount."
  (if (eq b 0) (error "Div by 0!")
    (round (/ a b))))
```

## Parse you a LISP
```lisp
(defun div (a b)
  "Divide A by B and return rounded amount."
  (if (eq b 0) (error "Div by 0!")
    (round (/ a b))))
```
```haskell
EList [EAtom "defun", EAtom "div", EList [EAtom "a", EAtom "b"]
  , EString "Divide A by B and return rounded amount."
  , EList [EAtom "if", EList [EAtom "eq", EAtom "b", ENumber 0]
          , EList [EAtom "error", EString "Div by 0!"]
          , EList [EAtom "round"
                  , EList [EAtom "/", EAtom "a", EAtom "b"]]]]
```

# My War: MusicXML

## &nbsp;
![](img/mywar-xml.png)
![](img/henry-rollins.jpg)
![](img/mywar-music.png)

## The Problem

- I've generated a bunch of music data
- I need to perform it with humans
- I need to create decent sheet music
- Lilypond jumps the shark, MusicABC too trivial

## The "Solution"

- MusicXML, the "pro" way to go
- Lots of printing support
- In Java, use JAXB
- In Haskell ...

## Use a hand-rolled MusicXML emitter on hackage

- Only implements whatever the author got around to
- Only supports author's idiosyncratic music lib
- MusicXML v2.0 only

## Wait! I have a terrible idea

- MusicXML fully specified in XSD
- Ooh look a library to spit out DSLs from XSD!
- Ooh look it's been abandoned since 2011

_(rolls up sleeves)_

## Let's generate a MusicXML library from XSD

- Generate a DSL that will always correctly spit out XML
- Load XSD as an AST
- Need to "recognize" streams of XSD XML

## Huh, that sounds a lot like a ... parser

```haskell
-- | Recognize attribute group
attributeGroup :: XParser m => m AttributeGroup
attributeGroup = do
  atEl (xsName "attributeGroup")
  AttributeGroup . qn <$> attr (name "name")
        <*> attrs <*> documentation
     <|> (AttributeGroupRef . Unresolved . qn)
        <$> attr (name "ref")

-- | Recognize attributes and attributeGroups
--   (which often come together).
attrs :: XParser m => m Attributes
attrs = Attributes <$>
    findChildren (xsName "attribute") attribute <*>
    findChildren (xsName "attributeGroup") attributeGroup
```

# XML element parser combinator

## Take 1: StateT List + ExceptT

```haskell
-- | XParser constraint kind. Stack state + alternative + errors.
type XParser m =
  (Alternative m, MonadState [X.Element] m, MonadError String m)

-- | run XParser on an element.
parseX :: (Monad m) => StateT [X.Element] (ExceptT String m) b
       -> X.Element -> m (Either String b)
parseX sel e = runExceptT (evalStateT sel [e])
```
