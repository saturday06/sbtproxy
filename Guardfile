notification :off

guard :haskell do
  watch(%r{test/.+\.l?hs$})
  watch(%r{.+\.l?hs$})
  watch(%r{\.cabal$})
  callback(:start_begin) { `cabal configure --enable-tests` }
end
