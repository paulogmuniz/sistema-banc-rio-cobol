📁 sistema-bancario-cobol/
├── cliente.cbl              --> Cadastro de cliente
├── conta.cbl                --> Abertura de conta
├── operacoes.cbl            --> Depósito, Saque e Saldo
├── relatorios.cbl           --> Geração de relatórios
├── dados-clientes.txt       --> Armazena dados dos clientes
├── dados-contas.txt         --> Armazena dados das contas
├── README.md                --> Documentação
IDENTIFICATION DIVISION.
       PROGRAM-ID. CLIENTE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIENTES ASSIGN TO "dados-clientes.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CLIENTES.
       01 CLIENTE-REGISTRO.
           05 CPF              PIC X(11).
           05 NOME             PIC X(30).

       WORKING-STORAGE SECTION.
       01 CPF-ENTRADA         PIC X(11).
       01 NOME-ENTRADA        PIC X(30).
       01 OPC                 PIC 9.
       01 EOF                 PIC X VALUE "N".
       01 ENCONTRADO          PIC X VALUE "N".

       01 CLIENTE-LIDO.
           05 CPF-LIDO        PIC X(11).
           05 NOME-LIDO       PIC X(30).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "===== CADASTRO DE CLIENTES ====="
           PERFORM UNTIL OPC = 2
               DISPLAY "1 - Cadastrar novo cliente"
               DISPLAY "2 - Sair"
               ACCEPT OPC

               EVALUATE OPC
                   WHEN 1
                       PERFORM CADASTRAR-CLIENTE
                   WHEN 2
                       DISPLAY "Saindo do cadastro..."
                   WHEN OTHER
                       DISPLAY "Opcao invalida."
               END-EVALUATE
           END-PERFORM
           STOP RUN.

       CADASTRAR-CLIENTE.
           DISPLAY "Digite o CPF (somente numeros): "
           ACCEPT CPF-ENTRADA
           DISPLAY "Digite o nome do cliente: "
           ACCEPT NOME-ENTRADA

           OPEN INPUT CLIENTES
           MOVE "N" TO ENCONTRADO
           PERFORM UNTIL EOF = "S"
               READ CLIENTES INTO CLIENTE-LIDO
                   AT END
                       MOVE "S" TO EOF
                   NOT AT END
                       IF CPF-ENTRADA = CPF-LIDO
                           MOVE "S" TO ENCONTRADO
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CLIENTES
           MOVE "N" TO EOF

           IF ENCONTRADO = "S"
               DISPLAY "Cliente ja cadastrado com esse CPF!"
           ELSE
               OPEN EXTEND CLIENTES
               MOVE CPF-ENTRADA TO CPF
               MOVE NOME-ENTRADA TO NOME
               WRITE CLIENTE-REGISTRO
               CLOSE CLIENTES
               DISPLAY "Cliente cadastrado com sucesso!"
           END-IF.
           IDENTIFICATION DIVISION.
       PROGRAM-ID. CONTA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIENTES ASSIGN TO "dados-clientes.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CONTAS ASSIGN TO "dados-contas.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CLIENTES.
       01 CLIENTE-ARQ.
           05 CPF-C           PIC X(11).
           05 NOME-C          PIC X(30).

       FD CONTAS.
       01 CONTA-ARQ.
           05 CPF-CONTA       PIC X(11).
           05 NUM-CONTA       PIC 9(6).
           05 SALDO           PIC 9(7)V99.

       WORKING-STORAGE SECTION.
       01 CPF-ENTRADA         PIC X(11).
       01 SALDO-INICIAL       PIC 9(7)V99.
       01 NUM-CONTA-AUTO      PIC 9(6) VALUE 100001.
       01 CONT-ENCONTRADO     PIC X VALUE "N".
       01 EOF                 PIC X VALUE "N".

       01 DADOS-LIDOS.
           05 CPF-LIDO        PIC X(11).
           05 NOME-LIDO       PIC X(30).

       01 CONTA-LIDA.
           05 CPF-LIDA        PIC X(11).
           05 NUM-LIDA        PIC 9(6).
           05 SALDO-LIDO      PIC 9(7)V99.

       PROCEDURE DIVISION.
       MAIN.
           DISPLAY "===== ABERTURA DE CONTA ====="
           DISPLAY "Digite o CPF do cliente: "
           ACCEPT CPF-ENTRADA

           OPEN INPUT CLIENTES
           MOVE "N" TO CONT-ENCONTRADO
           PERFORM UNTIL EOF = "S"
               READ CLIENTES INTO DADOS-LIDOS
                   AT END
                       MOVE "S" TO EOF
                   NOT AT END
                       IF CPF-ENTRADA = CPF-LIDO
                           MOVE "S" TO CONT-ENCONTRADO
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CLIENTES
           MOVE "N" TO EOF

           IF CONT-ENCONTRADO = "S"
               DISPLAY "Digite o saldo inicial: "
               ACCEPT SALDO-INICIAL

               PERFORM GERAR-NUMERO-CONTA
               OPEN EXTEND CONTAS
               MOVE CPF-ENTRADA TO CPF-CONTA
               MOVE NUM-CONTA-AUTO TO NUM-CONTA
               MOVE SALDO-INICIAL TO SALDO
               WRITE CONTA-ARQ
               CLOSE CONTAS

               DISPLAY "Conta criada com sucesso!"
               DISPLAY "Numero da conta: " NUM-CONTA-AUTO
           ELSE
               DISPLAY "CPF nao encontrado. Cadastre o cliente primeiro."
           END-IF
           STOP RUN.

       GERAR-NUMERO-CONTA.
           OPEN INPUT CONTAS
           PERFORM UNTIL EOF = "S"
               READ CONTAS INTO CONTA-LIDA
                   AT END
                       MOVE "S" TO EOF
                   NOT AT END
                       IF NUM-LIDA >= NUM-CONTA-AUTO
                           ADD 1 TO NUM-LIDA
                           MOVE NUM-LIDA TO NUM-CONTA-AUTO
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CONTAS
           MOVE "N" TO EOF.
           IDENTIFICATION DIVISION.
       PROGRAM-ID. OPERACOES.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONTAS ASSIGN TO "dados-contas.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CONTAS.
       01 REG-CONTA.
           05 CPF-CONTA     PIC X(11).
           05 NUM-CONTA     PIC 9(6).
           05 SALDO         PIC 9(7)V99.

       WORKING-STORAGE SECTION.
       01 OPCAO              PIC 9.
       01 NUM-ENTRADA        PIC 9(6).
       01 VALOR              PIC 9(7)V99.
       01 TEMP-SALDO         PIC 9(7)V99.
       01 EOF                PIC X VALUE "N".
       01 REG-LIDO.
           05 CPF-LIDO      PIC X(11).
           05 NUM-LIDO      PIC 9(6).
           05 SALDO-LIDO    PIC 9(7)V99.
       01 NOVA-CONTA.
           05 CPF-NOVO      PIC X(11).
           05 NUM-NOVO      PIC 9(6).
           05 SALDO-NOVO    PIC 9(7)V99.
       01 ENCONTRADO         PIC X VALUE "N".

       PROCEDURE DIVISION.
       MAIN.
           DISPLAY "===== OPERACOES BANCARIAS ====="
           PERFORM UNTIL OPCAO = 4
               DISPLAY "1 - Deposito"
               DISPLAY "2 - Saque"
               DISPLAY "3 - Consultar Saldo"
               DISPLAY "4 - Sair"
               ACCEPT OPCAO

               EVALUATE OPCAO
                   WHEN 1
                       PERFORM DEPOSITO
                   WHEN 2
                       PERFORM SAQUE
                   WHEN 3
                       PERFORM CONSULTA
                   WHEN 4
                       DISPLAY "Encerrando operacoes..."
                   WHEN OTHER
                       DISPLAY "Opcao invalida"
               END-EVALUATE
           END-PERFORM
           STOP RUN.

       DEPOSITO.
           DISPLAY "Digite o numero da conta: "
           ACCEPT NUM-ENTRADA
           DISPLAY "Valor do deposito: "
           ACCEPT VALOR
           MOVE "N" TO EOF
           MOVE "N" TO ENCONTRADO
           OPEN INPUT CONTAS
           OPEN OUTPUT TEMP-FILE
           PERFORM UNTIL EOF = "S"
               READ CONTAS INTO REG-LIDO
                   AT END
                       MOVE "S" TO EOF
                   NOT AT END
                       IF NUM-LIDO = NUM-ENTRADA
                           ADD VALOR TO SALDO-LIDO
                           MOVE "S" TO ENCONTRADO
                       END-IF
                       WRITE REG-LIDO
               END-READ
           END-PERFORM
           CLOSE CONTAS
           IF ENCONTRADO = "S"
               DISPLAY "Deposito realizado com sucesso!"
           ELSE
               DISPLAY "Conta nao encontrada."
           END-IF.

       SAQUE.
           DISPLAY "Digite o numero da conta: "
           ACCEPT NUM-ENTRADA
           DISPLAY "Valor do saque: "
           ACCEPT VALOR
           MOVE "N" TO EOF
           MOVE "N" TO ENCONTRADO
           OPEN INPUT CONTAS
           OPEN OUTPUT TEMP-FILE
           PERFORM UNTIL EOF = "S"
               READ CONTAS INTO REG-LIDO
                   AT END
                       MOVE "S" TO EOF
                   NOT AT END
                       IF NUM-LIDO = NUM-ENTRADA
                           IF SALDO-LIDO >= VALOR
                               SUBTRACT VALOR FROM SALDO-LIDO
                               MOVE "S" TO ENCONTRADO
                           ELSE
                               DISPLAY "Saldo insuficiente!"
                           END-IF
                       END-IF
                       WRITE REG-LIDO
               END-READ
           END-PERFORM
           CLOSE CONTAS
           IF ENCONTRADO = "S"
               DISPLAY "Saque realizado com sucesso!"
           ELSE
               DISPLAY "Conta nao encontrada ou saldo insuficiente."
           END-IF.

       CONSULTA.
           DISPLAY "Digite o numero da conta: "
           ACCEPT NUM-ENTRADA
           MOVE "N" TO EOF
           MOVE "N" TO ENCONTRADO
           OPEN INPUT CONTAS
           PERFORM UNTIL EOF = "S"
               READ CONTAS INTO REG-LIDO
                   AT END
                       MOVE "S" TO EOF
                   NOT AT END
                       IF NUM-LIDO = NUM-ENTRADA
                           DISPLAY "CPF: " CPF-LIDO
                           DISPLAY "Saldo atual: R$ " SALDO-LIDO
                           MOVE "S" TO ENCONTRADO
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CONTAS
           IF ENCONTRADO NOT = "S"
               DISPLAY "Conta nao encontrada."
           END-IF.
           IDENTIFICATION DIVISION.
       PROGRAM-ID. RELATORIOS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIENTES ASSIGN TO "dados-clientes.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CONTAS ASSIGN TO "dados-contas.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CLIENTES.
       01 CLIENTE-REG.
           05 CPF-C        PIC X(11).
           05 NOME-C       PIC X(30).

       FD CONTAS.
       01 CONTA-REG.
           05 CPF-CONTA    PIC X(11).
           05 NUM-CONTA    PIC 9(6).
           05 SALDO        PIC 9(7)V99.

       WORKING-STORAGE SECTION.
       01 OPC             PIC 9.
       01 CPF-ENTRADA     PIC X(11).
       01 EOF             PIC X VALUE "N".

       01 LIDO-C.
           05 CPF-LIDO    PIC X(11).
           05 NOME-LIDO   PIC X(30).

       01 LIDO-CT.
           05 CPF-CT-LIDO PIC X(11).
           05 NUM-CT-LIDO PIC 9(6).
           05 SALDO-LIDO  PIC 9(7)V99.

       PROCEDURE DIVISION.
       MAIN.
           DISPLAY "===== RELATORIOS ====="
           PERFORM UNTIL OPC = 4
               DISPLAY "1 - Listar todos os clientes"
               DISPLAY "2 - Listar todas as contas"
               DISPLAY "3 - Consultar contas por CPF"
               DISPLAY "4 - Sair"
               ACCEPT OPC

               EVALUATE OPC
                   WHEN 1
                       PERFORM RELATORIO-CLIENTES
                   WHEN 2
                       PERFORM RELATORIO-CONTAS
                   WHEN 3
                       PERFORM RELATORIO-POR-CPF
                   WHEN 4
                       DISPLAY "Encerrando relatorios..."
                   WHEN OTHER
                       DISPLAY "Opcao invalida."
               END-EVALUATE
           END-PERFORM
           STOP RUN.

       RELATORIO-CLIENTES.
           DISPLAY "----- LISTA DE CLIENTES -----"
           MOVE "N" TO EOF
           OPEN INPUT CLIENTES
           PERFORM UNTIL EOF = "S"
               READ CLIENTES INTO LIDO-C
                   AT END
                       MOVE "S" TO EOF
                   NOT AT END
                       DISPLAY "CPF: " CPF-LIDO ", Nome: " NOME-LIDO
               END-READ
           END-PERFORM
           CLOSE CLIENTES
           MOVE "N" TO EOF.

       RELATORIO-CONTAS.
           DISPLAY "----- LISTA DE CONTAS -----"
           MOVE "N" TO EOF
           OPEN INPUT CONTAS
           PERFORM UNTIL EOF = "S"
               READ CONTAS INTO LIDO-CT
                   AT END
                       MOVE "S" TO EOF
                   NOT AT END
                       DISPLAY "Conta: " NUM-CT-LIDO ", CPF: " CPF-CT-LIDO
                       DISPLAY "Saldo: R$ " SALDO-LIDO
               END-READ
           END-PERFORM
           CLOSE CONTAS
           MOVE "N" TO EOF.

       RELATORIO-POR-CPF.
           DISPLAY "Digite o CPF para consultar contas: "
           ACCEPT CPF-ENTRADA
           MOVE "N" TO EOF
           OPEN INPUT CONTAS
           PERFORM UNTIL EOF = "S"
               READ CONTAS INTO LIDO-CT
                   AT END
                       MOVE "S" TO EOF
                   NOT AT END
                       IF CPF-CT-LIDO = CPF-ENTRADA
                           DISPLAY "Conta: " NUM-CT-LIDO ", Saldo: R$ " SALDO-LIDO
                       END-IF
               END-READ
           END-PERFORM
           CLOSE CONTAS
           MOVE "N" TO EOF.
