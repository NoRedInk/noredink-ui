def node: "    at \(.target | join(" ")):\n\n      \(.failureSummary | gsub("\n"; "\n      "))";
def violation: "  \(.id): \(.impact) violation with \(.nodes | length) instances.\n\n  \(.help) (\(.helpUrl))\n\n\(.nodes | map(node) | join("\n\n"))";

"Tested \(.url) with \(.testEngine.name) \(.testEngine.version) at \(.timestamp)

Agent information:

  \(.testEnvironment | to_entries | map("\(.key): \(.value)") | join("\n  "))

Summary: \(.passes | length) passes | \(.violations | length) violations | \(.incomplete | length) incomplete | \(.inapplicable | length) inapplicable

Violations:

\(.violations | map(violation) | join("\n\n"))
"