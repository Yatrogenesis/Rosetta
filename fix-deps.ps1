$crates = @(
    'rosetta-core', 'rosetta-ir', 'rosetta-codegen', 'rosetta-c', 'rosetta-wasm',
    'rosetta-fortran', 'rosetta-cobol', 'rosetta-pli', 'rosetta-lisp', 'rosetta-quickbasic', 'rosetta-ml',
    'rosetta-mumps', 'rosetta-rpg', 'rosetta-rexx',
    'rosetta-pascal', 'rosetta-algol', 'rosetta-modula2',
    'rosetta-ada', 'rosetta-apl', 'rosetta-snobol',
    'rosetta-simula', 'rosetta-smalltalk', 'rosetta-forth',
    'rosetta-planner', 'rosetta-ops5', 'rosetta-krl', 'rosetta-prolog', 'rosetta-clips',
    'rosetta-cli', 'rosetta-validator', 'rosetta-gui', 'rosetta-docs'
)

$depNames = @(
    'rosetta-core', 'rosetta-ir', 'rosetta-codegen', 'rosetta-c', 'rosetta-wasm',
    'rosetta-fortran', 'rosetta-cobol', 'rosetta-pli', 'rosetta-lisp', 'rosetta-quickbasic', 'rosetta-ml',
    'rosetta-mumps', 'rosetta-rpg', 'rosetta-rexx',
    'rosetta-pascal', 'rosetta-algol', 'rosetta-modula2',
    'rosetta-ada', 'rosetta-apl', 'rosetta-snobol',
    'rosetta-simula', 'rosetta-smalltalk', 'rosetta-forth',
    'rosetta-planner', 'rosetta-ops5', 'rosetta-krl', 'rosetta-prolog', 'rosetta-clips',
    'rosetta-cli', 'rosetta-validator', 'rosetta-gui', 'rosetta-docs'
)

foreach ($crate in $crates) {
    $path = "C:\Users\pakom\Rosetta\crates\$crate\Cargo.toml"
    if (Test-Path $path) {
        $content = Get-Content $path -Raw

        # Replace each dependency reference
        foreach ($dep in $depNames) {
            $newDep = $dep -replace 'rosetta-', 'rosetta-stone-'
            # Format: rosetta-xxx = { workspace = true }
            $content = $content -replace "$dep = \{ workspace = true \}", "$newDep = { workspace = true }"
            # Format: rosetta-xxx = { workspace = true, ... }
            $content = $content -replace "$dep = \{ workspace = true,", "$newDep = { workspace = true,"
            # Format: rosetta-xxx.workspace = true
            $content = $content -replace "$dep\.workspace = true", "$newDep.workspace = true"
        }

        Set-Content $path $content -NoNewline
        Write-Host "Fixed deps in: $crate"
    }
}
Write-Host "Done!"
