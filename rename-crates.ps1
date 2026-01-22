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

foreach ($crate in $crates) {
    $path = "C:\Users\pakom\Rosetta\crates\$crate\Cargo.toml"
    if (Test-Path $path) {
        $content = Get-Content $path -Raw
        # Replace crate name
        $newName = $crate -replace 'rosetta-', 'rosetta-stone-'
        $content = $content -replace "name = `"$crate`"", "name = `"$newName`""
        # Replace all workspace dependencies
        $content = $content -replace 'rosetta-core\.workspace', 'rosetta-stone-core.workspace'
        $content = $content -replace 'rosetta-ir\.workspace', 'rosetta-stone-ir.workspace'
        $content = $content -replace 'rosetta-codegen\.workspace', 'rosetta-stone-codegen.workspace'
        $content = $content -replace 'rosetta-fortran\.workspace', 'rosetta-stone-fortran.workspace'
        $content = $content -replace 'rosetta-cobol\.workspace', 'rosetta-stone-cobol.workspace'
        $content = $content -replace 'rosetta-lisp\.workspace', 'rosetta-stone-lisp.workspace'
        $content = $content -replace 'rosetta-quickbasic\.workspace', 'rosetta-stone-quickbasic.workspace'
        $content = $content -replace 'rosetta-ml\.workspace', 'rosetta-stone-ml.workspace'
        $content = $content -replace 'rosetta-planner\.workspace', 'rosetta-stone-planner.workspace'
        $content = $content -replace 'rosetta-ops5\.workspace', 'rosetta-stone-ops5.workspace'
        $content = $content -replace 'rosetta-krl\.workspace', 'rosetta-stone-krl.workspace'
        $content = $content -replace 'rosetta-prolog\.workspace', 'rosetta-stone-prolog.workspace'
        $content = $content -replace 'rosetta-validator\.workspace', 'rosetta-stone-validator.workspace'
        $content = $content -replace 'rosetta-c\.workspace', 'rosetta-stone-c.workspace'
        $content = $content -replace 'rosetta-wasm\.workspace', 'rosetta-stone-wasm.workspace'
        $content = $content -replace 'rosetta-docs\.workspace', 'rosetta-stone-docs.workspace'
        $content = $content -replace 'rosetta-pli\.workspace', 'rosetta-stone-pli.workspace'
        $content = $content -replace 'rosetta-mumps\.workspace', 'rosetta-stone-mumps.workspace'
        $content = $content -replace 'rosetta-rpg\.workspace', 'rosetta-stone-rpg.workspace'
        $content = $content -replace 'rosetta-rexx\.workspace', 'rosetta-stone-rexx.workspace'
        $content = $content -replace 'rosetta-pascal\.workspace', 'rosetta-stone-pascal.workspace'
        $content = $content -replace 'rosetta-algol\.workspace', 'rosetta-stone-algol.workspace'
        $content = $content -replace 'rosetta-modula2\.workspace', 'rosetta-stone-modula2.workspace'
        $content = $content -replace 'rosetta-ada\.workspace', 'rosetta-stone-ada.workspace'
        $content = $content -replace 'rosetta-apl\.workspace', 'rosetta-stone-apl.workspace'
        $content = $content -replace 'rosetta-snobol\.workspace', 'rosetta-stone-snobol.workspace'
        $content = $content -replace 'rosetta-simula\.workspace', 'rosetta-stone-simula.workspace'
        $content = $content -replace 'rosetta-smalltalk\.workspace', 'rosetta-stone-smalltalk.workspace'
        $content = $content -replace 'rosetta-forth\.workspace', 'rosetta-stone-forth.workspace'
        $content = $content -replace 'rosetta-clips\.workspace', 'rosetta-stone-clips.workspace'
        $content = $content -replace 'rosetta-cli\.workspace', 'rosetta-stone-cli.workspace'
        $content = $content -replace 'rosetta-gui\.workspace', 'rosetta-stone-gui.workspace'
        Set-Content $path $content -NoNewline
        Write-Host "Updated: $crate -> $newName"
    } else {
        Write-Host "Not found: $path"
    }
}
Write-Host "Done!"
