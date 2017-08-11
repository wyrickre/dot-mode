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

  Scenario: Captures smex extended commands with minibuffer text
    Given I start an action chain
    And I press "M-x"
    And I type "insert-char"
    And I press "<return>"
    And I press "69"
    And I execute the action chain
    Then I should only see "ai"
    Given I press "C-."
    Then I should only see "aii"

Feature: Can override motion commands
  In order to include motion commands
  As a dot-mode user
  I use dot-mode-override

  Scenario: Override in the middle
    Given I clear the buffer
    And I press "C-l"
    And I start an action chain
    And I type "a"
    And I press "C-M-."
    And I press "C-b"
    And I type "b"
    And I execute the action chain
    Then I should only see "ba"
    And the cursor should be at point "2"
    Given I press "C-."
    Then the cursor should be at point "3"
    And I should only see "bbaa"

  Scenario: Override at beginning of command
    Given I clear the buffer
    And I insert:
      """
      First line
      Second line
      Third line
      """
    And I go to beginning of buffer
    And I start an action chain
    And I press "C-M-."
    And I press "C-n"
    And I press "C-M-."
    And I press "C-e"
    And I type " modified"
    And I execute the action chain
    Then I should only see:
      """
      First line
      Second line modified
      Third line
      """
    Given I press "C-."
    Then I should only see:
      """
      First line
      Second line modified
      Third line modified
      """


Feature: global-dot-mode does not recurse on error
  If an error occurs
  Dot mode does not recurse after attempting to repeat

  Scenario: Repeat an empty buffer
    Given I empty the command buffer
    And I press "C-."
    Then The last message should be "Nothing to repeat"

  Scenario: Repeat an empty buffer in global-dot-mode
    Given I turn on global-dot-mode
    And I press "C-."
    Then The last message should be "Nothing to repeat"
    And I press "C-."
    Then The last message should be "Nothing to repeat"
    And I press "C-."
    Then The last message should be "Nothing to repeat"

Feature: Accounts for universal-argument and digit-argument
  If I modify a command with arguments
  Dot mod should record that

  Scenario Outline: I start a modification with a digit-argument
    Given I clear the buffer
    And I press "C-l"
    And I start an action chain
    And I press "<four-argument>"
    And I type "a"
    And I execute the action chain
    Then I should only see "aaaa"
    Given I press "C-."
    Then I should only see "aaaaaaaa"

    Examples:
      | four-argument |
      | C-u           |
      | C-u 4         |
      | C-4           |

  Scenario Outline: I start a modification with negative-argument
    Given I clear the buffer
    And I insert "aaaa"
    Given I type "l"
    And I press "C-b"
    And I start an action chain
    And I press "<negative-argument>"
    And I press "C-d"
    And I execute the action chain
    Then I should only see "aaal"
    Given I press "C-."
    Then I should only see "aal"

    Examples:
      | negative-argument |
      | C--               |
      | C-u -             |
      | C-- 1             |
      | C-u - 1           |

  Scenario Outline: I use a digit-argument in the middle of a modification
    Given I clear the buffer
    And I press "C-l"
    And I start an action chain
    And I type "he"
    And I press "<four-argument>"
    And I type "lo"
    And I execute the action chain
    Then I should see "hellllo"
    Given I press "C-."
    Then I should only see "hellllohellllo"

    Examples:
      | four-argument |
      | C-u           |
      | C-u 4         |
      | C-4           |


  Scenario Outline: I use negative-argument in the middle of a modification
    Given I clear the buffer
    And I insert "abababab"
    And I go to point "5"
    And I start an action chain
    And I press "C-d"
    And I press "<negative-argument>"
    And I press "C-d"
    And I execute the action chain
    Then I should only see "ababab"
    Given I press "C-."
    Then I should only see "abab"


    Examples:
      | negative-argument |
      | C--               |
      | C-u -             |
      | C-- 1             |
      | C-u - 1           |


  # Override should always apply to the actual command, not to the
  # digit-argument.
  # i.e.   override digit-argument command == digit-argument override command
  # also, as example
  # override C-u 34 command  ==
  # C-u override 34 command  ==
  # C-u 3 override 4 command  ==
  # C-u 34 override command

  Scenario Outline: I use override with a digit argument
    Given I clear the buffer
    And I insert "ababababab"
    And I start an action chain
    And I press "<override-backwards-2>"
    And I press "C-k"
    And I press "<override-backwards-2>"
    And I press "C-k"
    And I execute the action chain
    Then I should only see "ababab"
    Given I press "C-."
    Then I should only see "ab"

    Examples:
      | override-backwards-2 |
      | C-M-. C-2 C-b        |
      | C-M-. C-u 2 C-b      |
      | C-M-. C-u - 2 C-f    |
      | C-2 C-M-. C-b        |
      | C-u C-M-. 2 C-b      |
      | C-u C-M-. - 2 C-f    |
      | C-u 2 C-M-. C-b      |
      | C-u - C-M-. 2 C-f    |
      | C-u - 2 C-M-. C-f    |
