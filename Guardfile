guard :haskell,
  all_on_start: true,
  all_on_pass: true,
  ghci_options: ["-ignore-dot-ghci", "-DTEST", "-itest/spec"],
  top_spec: "test/spec/Spec.hs" do
  watch(%r{test/spec/.+Spec\.l?hs$})
  watch(%r{src/.+\.l?hs$})
end
