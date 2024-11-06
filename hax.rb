module Kernel
  alias λ proc

  def ∑(*args)
    args.inject(&:+)
  end
end

