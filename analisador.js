const fs = require('fs');
const parser = require('./compilador.js');

try {
  console.log("Iniciando análise...");
  
  // Obtém o nome do arquivo a partir dos argumentos da linha de comando ou usa o padrão
  const fileName = process.argv[2] || 'entradas/entrada_05_tudo.txt';
  
  // Lê o arquivo de entrada
  const input = fs.readFileSync(fileName, 'utf8');
  console.log(`Arquivo ${fileName} lido com sucesso.`);
  
  console.log("Iniciando parse...");
  // Analisa a entrada
  const result = parser.parse(input);
  console.log('Análise concluída com sucesso!');
} catch (error) {
  console.error('Erro durante a análise:', error.message);
  if (error.hash) {
    console.error('Detalhes do erro:');
    console.error('  Token esperado:', error.hash.expected);
    console.error('  Token encontrado:', error.hash.token);
    console.error('  Linha:', error.hash.line);
    console.error('  Texto:', error.hash.text);
  }
}