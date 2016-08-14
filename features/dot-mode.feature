Feature: Repeats changes to buffer
  In order to cut down on typing
  As a user
  I want to be able to repeat my last change

  Scenario: Insert text twice
    Given I clear the buffer
    And I turn on dot-mode
    Given I type "Hello there this is a test"
    And I press "C-b"
    And I press "C-f"
    And I press "C-."
    Then I should see:
    """
    Hello there this is a testHello there this is a test
    """

  Scenario: Use commands twice
    Given I go to point "10"
    And I press "C-k"
    And I press "C-4 C-b"
    And I press "C-."
    Then I should only see:
    """
    Hello
    """

  Scenario: Captures extended commands
    Given I start an action chain
    And I press "M-x"
    And I type "backward-delete-char"
    And I execute the action chain
    Then I should see "Hell"
    Given I press "C-."
    Then I should only see "Hel"

  Scenario: Captures extended commands that prompt for input
    Given I start an action chain
    And I press "M-x"
    And I type "insert-char"
    And I press "<return>"
    And I press "69"
    And I execute the action chain
    Then I should see "Heli"

Feature: Integrates with smex
  In order to use both dot-mode and smex
  I want to not have to worry about it

  Scenario: Captures smex extended commands
    Given I load smex
    Then I should see "Hel"
