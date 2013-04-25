package Test {

    class TAP {

        method plan ($count) { say 1, "..", $count }

        method ok     ($count, $msg) { say [ "ok",     $count, ($msg || "") ].join(" ") }
        method not_ok ($count, $msg) { say [ "not ok", $count, ($msg || "") ].join(" ") }

        method diag (*@msg) { warn @msg.join }

        method bailout ($msg) { die $msg }
    }

    class Builder {

        has $!output = ^Test::TAP.new;
        has $!count  = 0;

        method plan ($count) { $!output.plan($count)  }
        method done_testing  { $!output.plan($!count) }

        method ok ($test, $msg?) {
            self.inc_count;
            ($test) 
                ? $!output.ok($!count, $msg) 
                : $!output.not_ok($!count, $msg);
        }

        method is ($got, $expected, $msg?) {
            self.inc_count;
            if (self.compare($got, $expected)) {
                $!output.ok($!count, $msg);
            } else {
                $!output.not_ok($!count, $msg);
                self.output_err($got, $expected, $msg); 
            }
        }

        method is_deeply(@_) {
            self.inc_count;
            if (self.compare_deeply(@_)) {
                $!output.ok($!count, @_[2]);
            } else {
                $!output.not_ok($!count, @_[2]);
                self.output_err(~@_[0], ~@_[1], @_[2]); 
            }
        }

        method diag ($msg) { $!output.diag($msg) }

        submethod inc_count { $!count = $!count + 1; }

        submethod output_err ($got, $expected, $msg) {
            $!output.diag( 
                "#  Failed test", ($msg || ""), "\n",
                "#    got:      ", ~$got,       "\n", 
                "#    expected: ", ~$expected
            );
        }

        submethod compare ($got, $expected) {
            if ($got.isa("Str")) {
                $got eq ~$expected;   
            } elsif ($got.isa("Int") || $got.isa("Num")) {
                $got == +$expected;
            } elsif ($got.isa("Bool")) {
                $got == ?$expected;
            } else {
                $!output.bailout("Can only compare Str, Int, Num and Bool objects");
            }
        }

        submethod compare_deeply(@_) {
            if (@_[0].isa("Array")) {
                self.compare_arrays(@_[0], @_[1]);
            } elsif (@_[0].isa("Hash")) {
                self.compare_hashes(@_[0], @_[1]);
            } else {
                $!output.bailout("Can only compare deeply Array and Hash objects");
            }
        }

        submethod compare_arrays(@got, @expected) {
            # poor man's deep-compare of arrays
            self.compare(@got.join("|"), @expected.join("|"));
        }

        submethod compare_hashes(%got, %expected) {
            # poor man's deep-compare of hashes
            self.compare(%got.pairs.join("|"), %expected.pairs.join("|"));
        }
    }

    package More {

        my $builder = ^Test::Builder.new;

        sub plan ($count) is export { $builder.plan($count) }
        sub done_testing  is export { $builder.done_testing }

        sub pass ($msg?) is export {
            $builder.ok(true, $msg);
        }

        sub fail ($msg?) is export {
            $builder.ok(false, $msg);
        }

        sub ok ($test, $msg?) is export {
            $builder.ok($test, $msg);
        }

        sub is ($got, $expected, $msg?) is export {
            $builder.is($got, $expected, $msg);
        }

        sub is_deeply (*@_) is export {
            $builder.is_deeply(@_);
        }

        sub diag ($msg) is export {
            $builder.diag($msg);
        }

    }

}
