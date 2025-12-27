import * as vscode from 'vscode';
import * as cp from 'child_process';
import * as path from 'path';

let outputChannel: vscode.OutputChannel;

export function activate(context: vscode.ExtensionContext) {
    outputChannel = vscode.window.createOutputChannel('Rosetta Transpiler');

    // Register commands
    context.subscriptions.push(
        vscode.commands.registerCommand('rosetta.transpileToRust', () => transpile('rust')),
        vscode.commands.registerCommand('rosetta.transpileToC', () => transpile('c')),
        vscode.commands.registerCommand('rosetta.transpileToWasm', () => transpile('wasm')),
        vscode.commands.registerCommand('rosetta.showIR', showIR),
        vscode.commands.registerCommand('rosetta.validateEquivalence', validateEquivalence)
    );

    // Register diagnostics
    const diagnosticCollection = vscode.languages.createDiagnosticCollection('rosetta');
    context.subscriptions.push(diagnosticCollection);

    // Register code lens provider for supported languages
    const supportedLanguages = [
        'fortran', 'cobol', 'mumps', 'rpg', 'rexx', 'lisp',
        'pascal', 'ada', 'apl', 'pli', 'quickbasic', 'prolog'
    ];

    for (const lang of supportedLanguages) {
        context.subscriptions.push(
            vscode.languages.registerCodeLensProvider(
                { language: lang },
                new RosettaCodeLensProvider()
            )
        );
    }

    outputChannel.appendLine('Rosetta Transpiler extension activated');
}

export function deactivate() {
    if (outputChannel) {
        outputChannel.dispose();
    }
}

async function transpile(target: 'rust' | 'c' | 'wasm') {
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
        vscode.window.showErrorMessage('No active editor');
        return;
    }

    const document = editor.document;
    const sourceFile = document.fileName;
    const sourceLanguage = detectSourceLanguage(document.languageId);

    if (!sourceLanguage) {
        vscode.window.showErrorMessage(`Unsupported source language: ${document.languageId}`);
        return;
    }

    // Save document first
    await document.save();

    const config = vscode.workspace.getConfiguration('rosetta');
    const cliPath = config.get<string>('cliPath') || 'rosetta';

    const outputExtension = getOutputExtension(target);
    const outputFile = sourceFile.replace(/\.[^.]+$/, outputExtension);

    outputChannel.show();
    outputChannel.appendLine(`Transpiling ${path.basename(sourceFile)} to ${target}...`);

    try {
        const result = await runRosettaCli(cliPath, [
            'transpile',
            '--source', sourceLanguage,
            '--target', target,
            '--input', sourceFile,
            '--output', outputFile
        ]);

        outputChannel.appendLine(result);
        outputChannel.appendLine(`Output written to: ${outputFile}`);

        // Open the generated file
        const outputDoc = await vscode.workspace.openTextDocument(outputFile);
        await vscode.window.showTextDocument(outputDoc, vscode.ViewColumn.Beside);

        vscode.window.showInformationMessage(`Successfully transpiled to ${target}`);

        // Auto-validate if enabled
        if (config.get<boolean>('autoValidate')) {
            await validateEquivalence();
        }
    } catch (error) {
        const errorMessage = error instanceof Error ? error.message : String(error);
        outputChannel.appendLine(`Error: ${errorMessage}`);
        vscode.window.showErrorMessage(`Transpilation failed: ${errorMessage}`);
    }
}

async function showIR() {
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
        vscode.window.showErrorMessage('No active editor');
        return;
    }

    const document = editor.document;
    const sourceFile = document.fileName;
    const sourceLanguage = detectSourceLanguage(document.languageId);

    if (!sourceLanguage) {
        vscode.window.showErrorMessage(`Unsupported source language: ${document.languageId}`);
        return;
    }

    await document.save();

    const config = vscode.workspace.getConfiguration('rosetta');
    const cliPath = config.get<string>('cliPath') || 'rosetta';

    outputChannel.show();
    outputChannel.appendLine(`Generating IR for ${path.basename(sourceFile)}...`);

    try {
        const result = await runRosettaCli(cliPath, [
            'ir',
            '--source', sourceLanguage,
            '--input', sourceFile,
            '--format', 'json'
        ]);

        // Show IR in a new document
        const irDoc = await vscode.workspace.openTextDocument({
            content: result,
            language: 'json'
        });
        await vscode.window.showTextDocument(irDoc, vscode.ViewColumn.Beside);

    } catch (error) {
        const errorMessage = error instanceof Error ? error.message : String(error);
        outputChannel.appendLine(`Error: ${errorMessage}`);
        vscode.window.showErrorMessage(`Failed to generate IR: ${errorMessage}`);
    }
}

async function validateEquivalence() {
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
        vscode.window.showErrorMessage('No active editor');
        return;
    }

    const document = editor.document;
    const sourceFile = document.fileName;

    const config = vscode.workspace.getConfiguration('rosetta');
    const cliPath = config.get<string>('cliPath') || 'rosetta';

    outputChannel.show();
    outputChannel.appendLine(`Validating numerical equivalence for ${path.basename(sourceFile)}...`);

    try {
        const result = await runRosettaCli(cliPath, [
            'validate',
            '--input', sourceFile
        ]);

        outputChannel.appendLine(result);

        if (result.includes('PASS') || result.includes('equivalent')) {
            vscode.window.showInformationMessage('Numerical equivalence validated');
        } else {
            vscode.window.showWarningMessage('Numerical equivalence validation found differences');
        }
    } catch (error) {
        const errorMessage = error instanceof Error ? error.message : String(error);
        outputChannel.appendLine(`Error: ${errorMessage}`);
        vscode.window.showErrorMessage(`Validation failed: ${errorMessage}`);
    }
}

function runRosettaCli(cliPath: string, args: string[]): Promise<string> {
    return new Promise((resolve, reject) => {
        const process = cp.spawn(cliPath, args);
        let stdout = '';
        let stderr = '';

        process.stdout.on('data', (data) => {
            stdout += data.toString();
        });

        process.stderr.on('data', (data) => {
            stderr += data.toString();
        });

        process.on('close', (code) => {
            if (code === 0) {
                resolve(stdout);
            } else {
                reject(new Error(stderr || `Process exited with code ${code}`));
            }
        });

        process.on('error', (error) => {
            reject(error);
        });
    });
}

function detectSourceLanguage(languageId: string): string | null {
    const languageMap: { [key: string]: string } = {
        'fortran': 'fortran',
        'cobol': 'cobol',
        'mumps': 'mumps',
        'rpg': 'rpg',
        'rexx': 'rexx',
        'lisp': 'lisp',
        'pascal': 'pascal',
        'ada': 'ada',
        'apl': 'apl',
        'snobol': 'snobol',
        'algol': 'algol',
        'simula': 'simula',
        'pli': 'pli',
        'quickbasic': 'quickbasic',
        'ml': 'ml',
        'prolog': 'prolog',
        'planner': 'planner',
        'ops5': 'ops5',
        'krl': 'krl',
        'clips': 'clips'
    };

    return languageMap[languageId] || null;
}

function getOutputExtension(target: string): string {
    switch (target) {
        case 'rust': return '.rs';
        case 'c': return '.c';
        case 'wasm': return '.wat';
        default: return '.out';
    }
}

class RosettaCodeLensProvider implements vscode.CodeLensProvider {
    provideCodeLenses(document: vscode.TextDocument): vscode.CodeLens[] {
        const codeLenses: vscode.CodeLens[] = [];
        const topOfDocument = new vscode.Range(0, 0, 0, 0);

        codeLenses.push(
            new vscode.CodeLens(topOfDocument, {
                title: '$(rocket) Transpile to Rust',
                command: 'rosetta.transpileToRust'
            }),
            new vscode.CodeLens(topOfDocument, {
                title: '$(file-code) Transpile to C',
                command: 'rosetta.transpileToC'
            }),
            new vscode.CodeLens(topOfDocument, {
                title: '$(globe) Transpile to WASM',
                command: 'rosetta.transpileToWasm'
            }),
            new vscode.CodeLens(topOfDocument, {
                title: '$(json) Show IR',
                command: 'rosetta.showIR'
            })
        );

        return codeLenses;
    }
}
