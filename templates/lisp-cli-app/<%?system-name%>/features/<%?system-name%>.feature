Feature: Update header

  In order to fullfill its main functionality, <%?system-name%> should
  update the headers of all files passed to it as args.
  
  Scenario: update Makefile
    Given a file named "test-input" with:
    """
    eins
    zwei
    drei
    """
    When I run "../../<%?system-name%> test-input"
    Then the exit status should be 0
    And the file "test-input-new" should contain exactly:
    """
    eins
    2
    drei
    """
