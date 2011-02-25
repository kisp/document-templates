Feature: Basic invocation

  In order to behave like a normal command-line app, I'd like
  <%?system-name%> to react to -h and -v in the standard way.

  Scenario: -h: exit code 0
    When I run "../../<%?system-name%> -h"
    Then the exit status should be 0

  Scenario: command with no args: exit code 1
    When I run "../../<%?system-name%>"
    Then the exit status should be 1

  Scenario: -v
    When I run "../../<%?system-name%> -V"
    Then the output should contain "<%?system-name%> version"

  Scenario: -h text
    When I run "../../<%?system-name%> -h"
    Then the output should contain "usage: <%?system-name%>"

  Scenario: command with no args: -h text
    When I run "../../<%?system-name%>"
    Then the output should contain "usage: <%?system-name%>"

  Scenario: --hell: exit code 1
    When I run "../../<%?system-name%> -l"
    Then the exit status should be 1
