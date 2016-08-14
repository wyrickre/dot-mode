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

Feature: Captures extended commands

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
    Then I should only see "Heli"
    Given I press "C-."
    Then I should only see "Helii"

  Scenario: Works no matter what key execute-extended-command is bound to
    Given I bind "execute-extended-command" to "C-'"
    And I start an action chain
    And I press "C-'"
    And I type "insert-char"
    And I press "<return>"
    And I press "69"
    And I execute the action chain
    Then I should only see "Heliii"
    Given I press "C-."
    Then I should only see "Heliiii"


Feature: Ignores undo
  To keep things understandable
  Dot mode should ignore undo commands

  Scenario: Ignores the undo command
    Given I clear the buffer
    # Press a command to break apart the current command
    And I press "C-l"
    And I type "Goodbye"
    Then I should only see "Goodbye"
    Given I press "C-/"
    Then the buffer should be empty
    Given I press "C-."
    Then I should only see "Goodbye"

  Scenario: Ignores undo-tree-undo and undo-tree-redo
    Given I load "undo-tree"
    And I activate undo-tree
    And I clear the buffer
    And I press "C-l"
    And I type "Goodbye"
    Given I press "C-/"
    Then the buffer should be empty
    Given I press "C-."
    Then I should only see "Goodbye"

Feature: Integrates with smex
  In order to use both dot-mode and smex
  I want to not have to worry about it

  Scenario: Captures smex extended commands
    Given I load "smex"
    And I bind M-x to smex
    And I clear the buffer
    And I insert "aaa"
    Then I should only see "aaa"
    Given I start an action chain
    And I press "M-x"
    And I type "backward-delete-char"
    And I execute the action chain
    Then I should only see "aa"
    Given I press "C-."
    Then I should only see "a"

Feature: Can override motion commands
  In order to include motion commands
  As a dot-mode user
  I use dot-mode-override

  Scenario: Override backwards-char
    Given I clear the buffer
    And I press "C-l"
    And I start an action chain
    And I press "a"
    And I press "C-M-."
    And I press "C-b"
    And I press "b"
    And I execute the action chain
    Then I should only see "ba"
    And the cursor should be at point "2"
    Given I press "C-."
    Then the cursor should be at point "3"
    And I should only see "bbaa"
