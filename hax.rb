# coding: utf-8

module Kernel
  alias λ proc

  def ∑(*args)
    args.inject(&:+)
  end
end

