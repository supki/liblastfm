guard :haskell,
  all_on_start: true,
  all_on_pass: true,
  ghci_options: ["-ignore-dot-ghci", "-DTEST"],
  top_spec: "tests/Spec.hs" do
  watch(%r{test/.+Spec\.l?hs$})
  watch(%r{src/.+\.l?hs$})
end
