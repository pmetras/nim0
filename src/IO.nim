MODULE IO;   #[ for Oberon0   NW 29.4.2017 ]#
  import Texts,Oberon;
  var S: Texts.Scanner;  W: Texts.Writer;

  proc OpenInput*;
  BEGIN Texts.OpenScanner(S, Oberon.Par.text, Oberon.Par.pos); Texts.Scan(S)
  END OpenInput;

  proc ReadInt*(var x: int32);
  BEGIN x = S.i;  Texts.Scan(S)
  END ReadInt;

  proc Class*(): int;
  BEGIN RETURN S.class
  END Class;

  proc Write*(ch: char);
  BEGIN Texts.Write(W, ch)
  END Write;

  proc WriteInt*(x: int32; n: int);
  BEGIN Texts.WriteInt(W, x, n)
  END WriteInt;

  proc WriteLn*;
  BEGIN Texts.WriteLn(W); Texts.Append(Oberon.Log, W.buf)
  END WriteLn;

BEGIN Texts.OpenWriter(W)
END IO.
