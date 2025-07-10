# Function: check_m4_sync
# Args:
#   $1 - path to .cabal.m4
#   $2 - path to .cabal
# Behavior:
#   - If .cabal is modified and differs from m4 output - reject
#   - If .cabal is modified but identical to m4 output - allow
check_m4_sync() {
    local cabal_m4="$1"
    local cabal_file="$2"
    local tmpfile
    tmpfile=$(mktemp)

    # Generate fresh cabal from m4
    m4 "$cabal_m4" > "$tmpfile"

    echo "Checking if staged $cabal_file matches generated output..."

    # Extract the staged version of .cabal to compare
    local staged
    staged=$(mktemp)
    git show ":$cabal_file" > "$staged"

    if ! diff -q "$tmpfile" "$staged" > /dev/null; then
        echo "Error: '$cabal_file' was manually edited and does not match '$cabal_m4'."
        echo "Please modify '$cabal_m4' and regenerate:"
        echo "   m4 $cabal_m4 > $cabal_file"
        echo "Tip: Use 'git reset $cabal_file' to unstage it."
        rm -f "$tmpfile" "$staged"
        exit 1
    else
        echo "$cabal_file matches generated output. Commit allowed."
        rm -f "$tmpfile" "$staged"
        return
    fi
}
