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
    And I press "C-b"
    And I press "C-b"
    And I press "C-b"
    And I press "C-b"
    And I press "C-."
    Then I should see:
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
    Then I should see "Hel"
