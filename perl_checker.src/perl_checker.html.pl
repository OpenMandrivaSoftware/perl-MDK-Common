$s = <<'EOF';
<head><title>perl_checker</title></head>
<h1>Goals of perl_checker</h1>

<ul>
<li> for beginners in perl:
  based on what the programmer is writing,
 <ul>
  <li> suggest better or more standard ways to do the same
  <li> detect wrong code
  <br>
  =&gt; a kind of automatic teacher
 </ul>

<li> for senior programmers:
  detect typos, unused variables, check number
  of parameters, global analysis to check method calls...

<li> enforce the same perl style by enforcing a subset of perl of features.
     In perl <a href="http://c2.com/cgi/wiki?ThereIsMoreThanOneWayToDoIt">There is more than one way to do it</a>. 
     In perl_checker's subset of Perl, there is not too many ways to do it.
     This is especially useful for big projects.
     (NB: the subset is chosen to keep a good expressivity)

</ul>

<h1>Get it</h1>

<a href="http://cvs.mandrakesoft.com/cgi-bin/cvsweb.cgi/soft/perl-MDK-Common/perl_checker.src/">CVS source</a>

<h1>Implemented features</h1>

<dl>
 <dt>white space normalization
 <dd>enforce a similar coding style. In many languages you can find a coding
 style document (eg: <a href="http://www.gnu.org/prep/standards_23.html">the GNU one</a>).

 TESTS=force_layout.t

 </dd>
 <dt>disallow <i>complex</i> expressions
 <dd>perl_checker try to ban some weird-not-used-a-lot features.

 TESTS=syntax_restrictions.t

 </dd>
 <dt>suggest simpler expressions
 <dd>when there is a simpler way to write an expression, suggest it. It can
 also help detecting errors.

 TESTS=suggest_better.t

 </dd>
 <dt>context checks
 <dd>Perl has types associated with variables names, the so-called "context".
 Some expressions mixing contexts are stupid, perl_checker detects them.

 TESTS=context.t

 </dd>
 <dt>function call check
 <dd>detection of unknown functions or mismatching prototypes (warning: since
  perl is a dynamic language, some spurious warnings may occur when a function
  is defined using stashes).

 TESTS=prototype.t

 </dd>
 <dt>method call check
 <dd>detection of unknown methods or mismatching prototypes. perl_checker
 doesn't have any idea what the object type is, it simply checks if a method
 with that name and that number of parameters exists.

 TESTS=method.t

 </dd>
 <dt>return value check
 <dd>dropping the result of a functionnally <i>pure</i> function is stupid.
 using the result of a function returning void is stupid too.

 TESTS=return_value.t

 </dd>
 <dt>detect some Perl traps
 <dd>some Perl expressions are stupid, and one gets a warning when running
 them with <tt>perl -w</tt>. The drawback are <tt>perl -w</tt> is the lack of
 code coverage, it only detects expressions which are evaluated.

 TESTS=various_errors.t

</dl>

<h1>Todo</h1>

Functionalities that would be nice:
<ul>
 <li> add flow analysis
 <li> maybe a "soft typing" type analysis
 <li> detect places where imperative code can be replaced with
   functional code (already done for some <b>simple</b> loops)
 <li> check the number of returned values when checking prototype compliance
</ul>
EOF

my $_rationale = <<'EOF';
<h1>Rationale</h1>

Perl is a big language, there is <a
href="http://c2.com/cgi/wiki?ThereIsMoreThanOneWayToDoIt">ThereIsMoreThanOneWayToDoIt</a>.
It has advantages but also some drawbacks for team project:
<ul>
 <li> it is hard to learn every special rules. Automatically enforced syntax
 coding rules help learning incrementally
EOF

use lib ('test', '..');
use read_t;
sub get_example {
    my ($file) = @_;
    my @tests = read_t::read_t("test/$file");
    $file =~ s|test/||;
    qq(<p><a name="$file"><table border=1 cellpadding=3>\n) .
      join('', map { 
	  my $lines = join("<br>", map { "<tt>" . html_quote($_) . "</tt>" } @{$_->{lines}});
	  my $logs = join("<br>", map { html_quote($_) } @{$_->{logs}});
	  "  <tr><td>\n", $lines, "</td><td>", $logs, "</td></tr>\n";
      } @tests) .
      "</table></a>\n";
}

sub anchor_to_examples {
    my ($s) = @_;
    $s =~ s!TESTS=(\S+)!(<a href="#$1">examples</a>)!g;
    $s;
}
sub fill_in_examples {
    my ($s) = @_;
    $s =~ s!TESTS=(\S+)!get_example($1)!ge;
    $s;
}

$s =~ s!<h1>Implemented features</h1>(.*)<h1>!
        "<h1>Implemented features</h1>" . anchor_to_examples($1) .
        "<h1>Examples</h1>" . fill_in_examples($1) .
        "<h1>"!se;

print $s;

sub html_quote {
    local $_ = $_[0];
    s/</&lt;/g;
    s/>/&gt;/g;
    s/^(\s*)/"&nbsp;" x length($1)/e;
    $_;
}
