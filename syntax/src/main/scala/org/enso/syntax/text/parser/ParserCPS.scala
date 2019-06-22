package org.enso.syntax.text.parser

import java.io.Reader

// format: off

////////////
// Result //
////////////

//trait Result[IN, OUT]
//case class Partial[IN, OUT](cont: IN => Result[IN, OUT])                 extends Result[IN, OUT]
//case class Done   [IN, OUT](rest: IN, output: OUT)                       extends Result[IN, OUT]
//case class Fail   [IN, OUT](rest: IN, ctxs: List[String], error: String) extends Result[IN, OUT]
//
//case class Pos(offset: Int)
//
//trait More
//case object Complete   extends More
//case object Incomplete extends More
//
//class PP() {
//
//  type Failure[I, T, R] = (T, Pos, More, List[String], String) => Result[I, R]
//  type Success[I, T, A, R] = (T, Pos, More, A) => Result[I, R]
//
//
//  class ParserCPS[IN, OUT](
//    run: ( IN, Pos, More
//         , Failure[IN, IN /*State*/ , OUT /*R*/ ]
//         , Success[IN, IN /*State*/ , OUT, OUT /*R*/ ]
//         ) => Result[IN, OUT]) {
//
//    def flatmap[OUT2](f: OUT => ParserCPS[IN,OUT2]): ParserCPS[IN, OUT2] = {
//      new ParserCPS((t,pos,more,lose,succ) => {
//        val succ2 = (t2:IN,pos2:Pos,more2:More,a:OUT2) => {f(a).run(t2,pos2,more2,lose,succ) }
//        this.run(t,pos,more,lose,succ2)
//      })
//    }
//
//
////    m >>= k = Parser $ \t !pos more lose succ ->
////    let succ' t' !pos' more' a = runParser (k a) t' pos' more' lose succ
////      in runParser m t pos more lose succ'
//
//
//  }
//
//  // format: on
//
//  //class PP() {
//  //  type Input  = String
//  //  type Output = Int
//  //  type State  = String
//  //
//  //  type Failure =
//  //    (State, Pos, More, List[String], String) => Result[Input, Output]
//  //  type Success = (State, Pos, More, Output) => Result[Input, Output]
//  //
//  //  //failK :: Failure a
//  //  //failK t (Pos pos) _more stack msg = Fail (Buf.dropWord16 pos t) stack msg
//  def failK[A](
//    in: String,
//    pos: Pos,
//    more: More,
//    stack: List[String],
//    msg: String
//  ): Result[String, A] = Fail(in.drop(pos.offset), stack, msg)
//
//  def successK[A](
//    in: String,
//    pos: Pos,
//    more: More,
//    a: A
//  ): Result[String, A] = Done(in.drop(pos.offset), a)
//
//  def peek(): ParserCPS[String, Char] = {
//    new ParserCPS[String, Char]((t, pos, more, lose, succ) => {
//      if (t.length() > pos.offset) {
//        succ(t, pos, more, t.charAt(pos.offset))
//      }
//      else {
//        // FIXME
//        // else let succ' t' pos' more' bs' = succ t' pos' more' $! B.unsafeHead bs'
//        // in ensureSuspended 1 t pos more lose succ'
//        lose(t, pos, more, List(), ":(")
//      }
//    })
//  }
//
//  def advance(n: Int): ParserCPS[String, Unit] = {
//    new ParserCPS((t, pos, more, lose, succ) => {
//      succ(t, pos.copy(offset = pos.offset + n), more, Unit)
//    })
//  }
////  def satisfy(check: Char => Boolean): ParserCPS[String,Char] = {
////
////  }
////  satisfy :: (Word8 -> Bool) -> Parser Word8
////    satisfy p = do
////    h <- peekWord8'
////  if p h
////    then advance 1 >> return h
////  else fail "satisfy"
////  {-# INLINE satisfy #-}
////  peekWord8' :: Parser Word8
////  peekWord8' = T.Parser $ \t pos more lose succ ->
////  if lengthAtLeast pos 1 t
////    then succ t pos more (Buf.unsafeIndex t (fromPos pos))
////  else let succ' t' pos' more' bs' = succ t' pos' more' $! B.unsafeHead bs'
////  in ensureSuspended 1 t pos more lose succ'
//
//}
////
////  //successK :: Success a a
////  //successK t (Pos pos) _more a = Done (Buf.dropWord16 pos t) a
//
//////  ensure :: Int -> Parser (Pos, Text)
//////  ensure n = T.Parser $ \t pos more lose succ ->
//////  case lengthAtLeast pos n t of
//////  Just n' -> succ t pos more (n', substring pos n' t)
//////  -- The uncommon case is kept out-of-line to reduce code size:
//////    Nothing -> ensureSuspended n t pos more lose succ
////
//////  def ensure(n:Int): Parser2 =
//////
////  class Parser2(
////    run: (State, Pos, More, Failure, Success) => Result[Input, Output]) {
////
////    //parse :: Parser a -> Text -> Result a
////    //parse m s = runParser m (buffer s) 0 Incomplete failK successK
////    def parse(input: Input): Result[Input, Output] =
////      this.run(input, Pos(0), Incomplete, failK, successK)
////  }
////
//////  def satisfy(check: Char => Boolean): Parser2 = {}
////
//////  def fail(): Parser2 = {
//////    new Parser2((t, pos, more, lose, succ) => {
//////      lose(t, pos, more, List(), "bad")
//////    })
//////  }
//////} = Parser $ \t pos more lose _succ -> lose t pos more [] msg
//////    where msg = "Failed reading: " ++ err
//////  def peek(): Parser2 = {
//////    new Parser2(
//////      (
//////        state: State,
//////        pos: Pos,
//////        more: More,
//////        fail: Failure,
//////        succ: Success
//////      ) => {
//////        if(state.length() > 0) {
//////          succ(state, pos, more, state.head)
//////        }
//////    )
//////  }
////
//////peekWord8' :: Parser Word8
//////peekWord8' = T.Parser $ \t pos more lose succ ->
//////if lengthAtLeast pos 1 t
//////  then succ t pos more (Buf.unsafeIndex t (fromPos pos))
//////else let succ' t' pos' more' bs' = succ t' pos' more' $! B.unsafeHead bs'
//////in ensureSuspended 1 t pos more lose succ'
////
//////  satisfy :: (Word8 -> Bool) -> Parser Word8
//////    satisfy p = do
//////    h <- peekWord8'
//////  if p h
//////    then advance 1 >> return h
//////  else fail "satisfy"
////}
////
//////dasd
//////-- | Run a parser.
//////parse :: Parser a -> Text -> Result a
//////parse m s = runParser m (buffer s) 0 Incomplete failK successK
//////newtype Parser i a = Parser {
//////runParser :: forall r.
//////State i -> Pos -> More
//////-> Failure i (State i)   r
//////-> Success i (State i) a r
//////-> IResult i r
//////}
//////
//////type family State i
//////type instance State ByteString = B.Buffer
//////type instance State Text = T.Buffer
//////
//////type Failure i t   r = t -> Pos -> More -> [String] -> String
//////-> IResult i r
//////type Success i t a r = t -> Pos -> More -> a -> IResult i r
//////
//////-- | Have we read all available input?
//////data More = Complete | Incomplete
//////deriving (Eq, Show)
//////newtype Parser i a = Parser {
//////  runParser :: forall r.
//////  State i -> Pos -> More
//////  -> Failure i (State i)   r
//////  -> Success i (State i) a r
//////  -> IResult i r
//////}
