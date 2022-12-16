use v6.d;
# use trace;
my %FUNC;
my %FUNC-LINE;
my %FUNC-LINE-ARG;
my $FILE = @*ARGS.shift;

my &Brusk-ARG-IND = -> $s {
  "$s".comb.map({"⁰¹²³⁴⁵⁶⁷⁸⁹".index($_) // -1}).max
} 

grammar Brusk-Code {
  token TOP {
    ^ <line> $
  }

  rule line {
    <expression> | <diad-partial>
  }

  rule expression {
    <number> | <string> | <empty> | <diad> | <monad> | <func> | <arg-line> | <func-line> | <map>
  }

  rule map {
   'm' <diad-partial> <expression>
  }

  # TODO: curry these bad bois
  proto rule func-line {*}

  rule func-line:sym<0> {
    '₀' <expression> ** {+@*ARGS}
  }

  rule func-line:sym<1> {
    '₁' <expression> ** {%FUNC-LINE-ARG{1} // 69}
  }

  rule func-line:sym<2> {
    '₂' <expression> ** {%FUNC-LINE-ARG{2} // 69}
  }

  rule func-line:sym<3> {
    '₃' <expression> ** {%FUNC-LINE-ARG{3} // 69}
  }

  proto rule monad {*}

  rule monad:sym<iota> {
    'ι' <expression>
  }

  rule monad:sym<sigma> {
    'Σ' <expression>
  }

  proto rule func {*}


  rule func:sym<ternary> {
    '?' <expression> <expression> <expression>
  }

  rule diad {
    <diad-partial> <expression>
  }

  proto rule diad-partial {*}

  rule diad-partial:sym<cons> {
    ':' <expression>
  }

  rule diad-partial:sym<join> {
    'J' <expression>
  }

  rule diad-partial:sym<add> {
    '+' <expression>
  }

  rule diad-partial:sym<subtract> {
    '-' <expression>
  }

  rule diad-partial:sym<multiply> {
    '*' <expression>
  }

  rule diad-partial:sym<divide> {
    '/' <expression>
  }

  rule diad-partial:sym<modulo> {
    '%' <expression>
  }

  rule diad-partial:sym<concatenate> {
    '~' <expression>
  }

  rule diad-partial:sym<compose> {
    'o' <diad-partial> <diad-partial>
  }

  rule diad-partial:sym<const> {
    'K' <expression>
  }
  
  rule diad-partial:sym<equal> {
    '=' <expression>
  }

  token number {
    \d+
  }

  token string {
    '"' <string-inner> '"'
  }

  token string-inner {
    <-["]>+
  }

  token arg-line {
    <[⁰¹²³⁴⁵⁶⁷⁸⁹]>
  }


  token empty {
    'ø'
  }
}

grammar Brusk-CodeInit {

  token TOP {
    [<line> \n]* <line>+
  }

  token line {
    <-[\n]>+
  }
}


class Brusk-CodeInitC {
  method TOP($m) {
    for $m<line>.kv -> $ind, $match {
      %FUNC-LINE{$ind} = $match;
      %FUNC-LINE-ARG{$ind} = Brusk-ARG-IND($match) + 1;
    }
  }

}

class Brusk-Func {
  has &.FUNC is rw;
  has @.ARG is rw;

  submethod BUILD(:&func, :@arg, :@arg-line) {
    self.FUNC = &func;
    self.ARG = @arg;
  }
}

class Brusk-Parse {
  method TOP($m) {
    $m.make: $m<line>.ast;
  }

  method line($m) {
    $m.make: $m<expression> ?? $m<expression>.ast !! $m<diad-partial>.ast
  }

  method expression($m) {
    $m.make: $m<map> ?? $m<map>.ast !! $m<func> ?? $m<func>.ast !! $m<func-line> ?? $m<func-line>.ast !! $m<arg-line> ?? $m<arg-line>.ast !! $m<number> ?? $m<number>.ast !! $m<diad> ?? $m<diad>.ast !! $m<diad-partial> ?? $m<diad-partial>.ast !! $m<monad> ?? $m<monad>.ast !! $m<string> ?? $m<string>.ast !! $m<empty> ?? $m<empty>.ast !! $m<cons> ?? $m<cons>.ast !! (die "invalid expr")
  }

  method map($m) {
    $m.make: Brusk-Func.new(:func(&Brusk-map), :arg([$m<diad-partial>, $m<expression>]))
  }

  method empty($m) {
    $m.make: Brusk-Func.new(:func(&Brusk-empty), :arg([[],]))
  }

  method func-line:sym<0>($m) {
    $m.make: Brusk-Func.new(:func(&Brusk-func-line-zero), :arg($m<expression>))
  }

  method func-line:sym<1>($m) {
    $m.make: Brusk-Func.new(:func(&Brusk-func-line-one), :arg($m<expression>))
  }

  method func-line:sym<2>($m) {
    $m.make: Brusk-Func.new(:func(&Brusk-func-line-two), :arg($m<expression>))
  }

  method func-line:sym<3>($m) {
    $m.make: Brusk-Func.new(:func(&Brusk-func-line-three), :arg($m<expression>))
  }

  method arg-line($m) {
    my $a = Brusk-Func.new(:func(&Brusk-arg-line), :arg([Brusk-ARG-IND($m)]));
    $m.make: $a
  }

  method number($m) {
    $m.make: Brusk-Func.new(:func(&Brusk-number), :arg([$m.Real]))
  }

  method string($m) {
    $m.make: Brusk-Func.new(:func(&Brusk-string), :arg(["$m<string-inner>"]))
  }

  method diad($m) {
    my $a = $m<diad-partial>.ast;

    $m.make: Brusk-Func.new(:func($a.FUNC), :arg([|$a.ARG, $m<expression>]))
  }

  method diad-partial:sym<add>($m) {
    $m.make: Brusk-Func.new(:func(&Brusk-add), :arg([$m<expression>]))
  }

  method diad-partial:sym<multiply>($m) {
    $m.make: Brusk-Func.new(:func(&Brusk-multiply), :arg([$m<expression>]))
  }

  method diad-partial:sym<divide>($m) {
    $m.make: Brusk-Func.new(:func(&Brusk-divide), :arg([$m<expression>]))
  }

  method diad-partial:sym<equal>($m) {
    $m.make: Brusk-Func.new(:func(&Brusk-equal), :arg([$m<expression>]))
  }

  method diad-partial:sym<join>($m) {
    $m.make: Brusk-Func.new(:func(&Brusk-join), :arg([$m<expression>]))
  }

  method diad-partial:sym<cons>($m) {
    $m.make: Brusk-Func.new(:func(&Brusk-cons), :arg([$m<expression>]))
  }

  method diad-partial:sym<subtract>($m) {
    $m.make: Brusk-Func.new(:func(&Brusk-subtract), :arg([$m<expression>]))
  }

  method diad-partial:sym<const>($m) {
    my $a = $m<diad-partial>.ast;
    $m.make: Brusk-Func.new(:func(&Brusk-const), :arg([$m<expression>]))
  }

  method diad-partial:sym<modulo>($m) {
    $m.make: Brusk-Func.new(:func(&Brusk-modulo), :arg([$m<expression>]))
  }

  method monad:sym<iota>($m) {
    $m.make: Brusk-Func.new(:func(&Brusk-iota), :arg([$m<expression>]))
  }


  method monad:sym<sigma>($m) {
    $m.make: Brusk-Func.new(:func(&Brusk-sigma), :arg([$m<expression>]))
  }

  method func:sym<ternary>($m) {
    $m.make: Brusk-Func.new(:func(&Brusk-ternary), :arg($m<expression>))
  }
}

sub Brusk-code-parse($m) {
  Brusk-Code.parse(~$m, actions => Brusk-Parse).ast
}

sub Brusk-EVAL-ternary($func is copy ,@arg is copy, @arg-line) {
  for @arg.kv -> $i, $arg is copy {
    $func = $func($arg);
  }

  $func(@arg-line)
}

sub Brusk-EVAL($func is copy, @arg is copy, @arg-line) {
  if [&Brusk-sigma, &Brusk-ternary, &Brusk-add, &Brusk-subtract, &Brusk-multiply, &Brusk-divide, &Brusk-modulo, &Brusk-number, &Brusk-string, &Brusk-id, &Brusk-const, &Brusk-map, &Brusk-iota, &Brusk-arg-line, &Brusk-join, &Brusk-empty, &Brusk-cons, &Brusk-equal].any eqv $func {
    my $r = Brusk-EVAL-ternary($func, @arg, @arg-line);
    return $r;
  }
  for @arg.kv -> $i, $arg is copy {
    while $arg ~~ Match {
      my $f = $arg.ast;
      $arg = Brusk-EVAL($f.FUNC,$f.FUNC eqv &Brusk-arg-line ?? [$f.ARG[0]] !! $f.ARG, @arg-line);
    }
    if $func eqv &Brusk-arg-line {
      $func = Brusk-EVAL($func($arg), [@arg-line,], @arg-line)
    } elsif $func eqv &Brusk-func-line-zero {
      $func = Brusk-EVAL-ternary(%FUNC{0}.FUNC, %FUNC{0}.ARG, [$arg]);
    } elsif $func eqv &Brusk-func-line-one {
      if %FUNC{1}:!exists {
        %FUNC{1} = Brusk-code-parse(%FUNC-LINE{1});
      }
      $func = Brusk-EVAL-ternary(%FUNC{1}.FUNC, %FUNC{1}.ARG, [$arg]);
    } elsif $func eqv &Brusk-func-line-two {
      if %FUNC{2}:!exists {
        %FUNC{2} = Brusk-code-parse(%FUNC-LINE{2});
      }
      $func = Brusk-EVAL-ternary(%FUNC{2}.FUNC, %FUNC{2}.ARG, [$arg]);
    } elsif $func eqv &Brusk-func-line-three {
      if %FUNC{3}:!exists {
        %FUNC{3} = Brusk-code-parse(%FUNC-LINE{3});
      }
      $func = Brusk-EVAL-ternary(%FUNC{3}.FUNC, %FUNC{3}.ARG, [$arg]);
    } else {
      $func = $func($arg)
    }
  }

  $func
}

my &Brusk-arg-line = -> $a {
  -> @arg-line {
    @arg-line[$a]
  }
}

my &Brusk-id = -> $a {
  -> @arg-line {
    $a
  }
}

my &Brusk-add = -> $a {
  -> $b {
    -> @arg-line {
      Brusk-EVAL($a.ast.FUNC, $a.ast.ARG, @arg-line) + Brusk-EVAL($b.ast.FUNC, $b.ast.ARG, @arg-line)
    }
  }
}

my &Brusk-join = -> $a {
  -> $b {
    -> @arg-line {
      Brusk-EVAL($b.ast.FUNC, $b.ast.ARG, @arg-line).join(Brusk-EVAL($a.ast.FUNC, $a.ast.ARG, @arg-line))
    }
  }
}

my &Brusk-subtract = -> $a {
  -> $b {
    -> @arg-line {
      Brusk-EVAL($a.ast.FUNC, $a.ast.ARG, @arg-line) - Brusk-EVAL($b.ast.FUNC, $b.ast.ARG, @arg-line)
    }
  }
}

my &Brusk-multiply = -> $a {
  -> $b {
    -> @arg-line {
      Brusk-EVAL($a.ast.FUNC, $a.ast.ARG, @arg-line) * Brusk-EVAL($b.ast.FUNC, $b.ast.ARG, @arg-line)
    }
  }
}

my &Brusk-divide = -> $a {
  -> $b {
    -> @arg-line {
      Brusk-EVAL($a.ast.FUNC, $a.ast.ARG, @arg-line) / Brusk-EVAL($b.ast.FUNC, $b.ast.ARG, @arg-line)
    }
  }
}


my &Brusk-cons = -> $a {
  -> $list {
    -> @arg-line {
       [Brusk-EVAL($a.ast.FUNC, $a.ast.ARG, @arg-line), |Brusk-EVAL($list.ast.FUNC, $list.ast.ARG, @arg-line)]
    }
  }
}

my &Brusk-concatenate = -> $a {
  -> $b {
    -> @arg-line {
      Brusk-EVAL($a.ast.FUNC, $a.ast.ARG, @arg-line) ~ Brusk-EVAL($b.ast.FUNC, $b.ast.ARG, @arg-line)
    }
  }
}


my &Brusk-const = -> $a {
  -> $b {
    -> @arg-line {
      Brusk-EVAL($a.ast.FUNC, $a.ast.ARG, @arg-line)
    }
  }
}

my &Brusk-modulo = -> $a {
  -> $b {
    -> @arg-line {
      Brusk-EVAL($a.ast.FUNC, $a.ast.ARG, @arg-line) % Brusk-EVAL($b.ast.FUNC, $b.ast.ARG, @arg-line)
    }
  }
}


my &Brusk-iota = -> $a {
  -> @arg-line {
    [0 ..^ Brusk-EVAL($a.ast.FUNC, $a.ast.ARG, @arg-line)]
  }
}

my &Brusk-equal = -> $a {
  -> $b {
    -> @arg-line {
      Brusk-EVAL($a.ast.FUNC, $a.ast.ARG, @arg-line) == Brusk-EVAL($b.ast.FUNC, $b.ast.ARG, @arg-line) ?? 1 !! 0
    }
  }
}

my &Brusk-sigma = -> @l {
  -> @arg-line {
    @l.sum
  }
}

my &Brusk-func-line-zero = -> $f {$f};
my &Brusk-func-line-one = -> $f {$f}; 
my &Brusk-func-line-two = -> $f {$f}; 
my &Brusk-func-line-three = -> $f {$f}; 

my &Brusk-map = -> $func {
  -> $list {
    -> @arg-line {
      my @l = Brusk-EVAL($list.ast.FUNC, $list.ast.ARG, @arg-line);
      my $ff = $func.ast;
      @l[0].map(-> $x {
        my $a = Brusk-Code.parse($x, actions => Brusk-Parse);
        my $r = Brusk-EVAL($func.ast.FUNC, [|$func.ast.ARG, $a], [$a, @arg-line]);
        $r
      }).List
    }
  }
}

my &Brusk-number = -> $a {
  -> @arg-line {
    $a
  }
}

my &Brusk-string = -> $a {
  -> @arg-line {
    $a
  }
}

my &Brusk-empty = -> $a {
  -> @arg-line {
    $a
  }
}


my &Brusk-ternary = -> $a {
  -> $b {
    -> $cond {
      -> @arg-line {
        my $c = Brusk-EVAL($cond.ast.FUNC, $cond.ast.ARG, @arg-line);
        $c ?? Brusk-EVAL($a.ast.FUNC, $a.ast.ARG, @arg-line) !! Brusk-EVAL($b.ast.FUNC, $b.ast.ARG, @arg-line)
      }
    }
  }
}



@*ARGS = @*ARGS.map({
  my $f = Brusk-Code.parse($_, actions => Brusk-Parse).ast;
  Brusk-EVAL($f.FUNC, $f.ARG, [])
});

Brusk-CodeInit.parse($FILE.IO.lines.grep({$_ ne "" && !$_.starts-with("#")}).join("\n"), actions => Brusk-CodeInitC);
%FUNC{0} = Brusk-Code.parse(~%FUNC-LINE{0}, actions => Brusk-Parse).ast;

say Brusk-EVAL(%FUNC{0}.FUNC, %FUNC{0}.ARG, @*ARGS);