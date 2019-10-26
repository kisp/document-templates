Feature: informational

  As a user of document-templates
  I want to be able to query useful information about the app

  Scenario: print version
     When I run `document-templates --version`
     Then the exit status should be 0
     And the output should contain "document-templates, version 0.0.15"

  Scenario: print short usage
    When I run `document-templates`
    Then the exit status should be 1
    And the output should contain "-h  --help"
    And the output should contain "-V  --version"

  Scenario Outline: print help
    When I run `document-templates <arg>`
    Then the exit status should be 0
    And the output should contain "-h  --help"

    Examples:
    | arg     |
    | -h      |
    | --help  |
    | --hel   |
