#!/usr/bin/env ruby

require '<%(underscores ?gem-name)%>'
require 'test/unit'

class Test<%(camel-case ?gem-name)%> < Test::Unit::TestCase
  def test_dummy
    assert_equal(3, 1 + 2)
  end
end
