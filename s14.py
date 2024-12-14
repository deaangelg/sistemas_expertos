import tkinter as tk
from tkinter import messagebox
import matplotlib.pyplot as plt
import networkx as nx
from enum import Enum
from tkinter import filedialog
from tkinter import ttk

# Clases básicas para tokens y nodos del AST
class TokenType(Enum):
    Y = 'y'
    O = 'o'
    SI = 'si'
    ENTONCES = 'entonces'
    NOT = 'no'
    EOL = 'eol'
    PALABRA = 'palabra'

class Token:
    def __init__(self, tipo: TokenType, valor: str):
        self.tipo = tipo
        self.valor = valor

    def __str__(self):
        return self.valor

class Node:
    pass

class PropNode(Node):
    counter = 0  # Contador para asignar identificadores únicos
    props_map = {}  # Diccionario para mapear proposiciones a sus identificadores

    def __init__(self, token):
        self.tokens = [token]  # Lista para almacenar tokens
        self.value = token.valor
        self.truth_value = None
        if self.value not in PropNode.props_map:
            PropNode.counter += 1
            PropNode.props_map[self.value] = f"x{PropNode.counter}"
        self.id = PropNode.props_map[self.value]  # Asignar identificador único a las proposiciones simples

    def addToken(self, token):
        self.tokens.append(token)
        self.value = ' '.join([t.valor for t in self.tokens])
        if self.value not in PropNode.props_map:
            PropNode.counter += 1
            PropNode.props_map[self.value] = f"x{PropNode.counter}"
        self.id = PropNode.props_map[self.value]

    def __str__(self):
        return f"Prop: {self.id} ({self.truth_value})"

class NegNode(Node):
    def __init__(self, child):
        self.child = child
        self.truth_value = None

class OperatorType(Enum):
    AND = 'and'
    OR = 'or'
    IMPLICATION = 'implies'

class OperatorNode(Node):
    counter = 0  # Contador para asignar identificadores únicos a proposiciones complejas

    def __init__(self, operator, left, right):
        self.operator = operator
        self.left = left
        self.right = right
        self.truth_value = None
        OperatorNode.counter += 1
        self.id = f"R{OperatorNode.counter}"  # Asignar identificador único a proposiciones complejas

    def __str__(self):
        return f"Operator: {self.id} ({self.truth_value})"


base_conocimientos = []

def crear_lista_formulas(proposiciones):
    lista_formulas = []
    for proposicion in proposiciones:
        tokens = analisis_lexico(proposicion)
        ast = analizar_sintactico(tokens)
        if ast:
            formula_logica = construir_expresion_logica(construir_arbol_semantico(ast))
            lista_formulas.append(formula_logica)
    return lista_formulas


def guardar_base_conocimientos(proposiciones):
    global base_conocimientos
    base_conocimientos = proposiciones

def comparar_frase():
    frase = nueva_frase_texto.get().strip()
    if not frase:
        messagebox.showerror("Error", "Por favor, ingrese una frase para comparar.")
        return

    # Analizar la nueva frase
    tokens = analisis_lexico(frase)
    ast = analizar_sintactico(tokens)

    if ast:
        nueva_formula_logica = construir_expresion_logica(construir_arbol_semantico(ast))

        # Comparar la nueva fórmula lógica con la lista de fórmulas proposicionales
        if nueva_formula_logica in lista_formulas:
            messagebox.showinfo("Éxito", f"La frase '{frase}' es consistente con la base de conocimientos.")
        else:
            messagebox.showerror("Conflicto", f"La frase '{frase}' contradice la base de conocimientos.")
    else:
        messagebox.showerror("Error", "La frase no es sintácticamente correcta.")

# Función para construir expresiones lógicas
def construir_expresion_logica(nodo):
    """Construir la expresión lógica completa del nodo."""
    if isinstance(nodo, PropNode):
        return nodo.id
    elif isinstance(nodo, NegNode):
        return f"¬({construir_expresion_logica(nodo.child)})"
    elif isinstance(nodo, OperatorNode):
        operator_map = {
            OperatorType.OR: "∨",
            OperatorType.AND: "∧",
            OperatorType.IMPLICATION: "⇒"
        }
        left_expr = construir_expresion_logica(nodo.left)
        right_expr = construir_expresion_logica(nodo.right)
        return f"({left_expr} {operator_map[nodo.operator]} {right_expr})"
    
def abrir_ventanas(ast, arbol_semantico):
    # Dibujar el AST
    dibujar_ast(ast)
    # Dibujar el árbol semántico
    dibujar_arbol_semantico(arbol_semantico)
    # Mostrar tabla de asignaciones
    mostrar_tabla_asignaciones(ast)


def generar_botones_proposiciones(proposiciones):
    for widget in frame_botones.winfo_children():
        widget.destroy()  # Limpiar botones antiguos

    columnas = 4  # Número de columnas deseadas
    for i, proposicion in enumerate(proposiciones):
        proposicion = proposicion.strip()
        if not proposicion:
            continue

        # Analizador Léxico
        tokens = analisis_lexico(proposicion)

        # Analizador Sintáctico
        ast = analizar_sintactico(tokens)

        if ast:
            arbol_semantico = construir_arbol_semantico(ast)
            evaluar_arbol_semantico(arbol_semantico)

            # Crear botón para cada proposición
            boton = tk.Button(frame_botones, text=f"Regla {i+1}", command=lambda a=ast, s=arbol_semantico: abrir_ventanas(a, s))
            boton.grid(row=i // columnas, column=i % columnas, padx=5, pady=5)

def cargar_desde_archivo():
    filepath = tk.filedialog.askopenfilename(filetypes=[("Text Files", "*.txt")])
    if not filepath:
        return

    with open(filepath, 'r') as file:
        proposiciones = file.readlines()

    entrada_texto.delete("1.0", tk.END)  # Corregir el índice a "1.0"
    for proposicion in proposiciones:
        entrada_texto.insert(tk.END, proposicion.strip() + '\n')

    generar_botones_proposiciones(proposiciones)


def realizar_analisis_desde_archivo(proposiciones):
    lexico_text.delete(1.0, tk.END)
    lexico_text.insert(tk.END, "Tipo\tValor\n")
    sintactico_text.delete(1.0, tk.END)

    for proposicion in proposiciones:
        proposicion = proposicion.strip()
        if not proposicion:
            continue

        # Analizador Léxico
        tokens = analisis_lexico(proposicion)
        for token in tokens:
            lexico_text.insert(tk.END, f"{token.tipo}\t{token.valor}\n")

        # Analizador Sintáctico
        ast = analizar_sintactico(tokens)

        if ast:
            sintactico_text.insert(tk.END, f"La proposición '{proposicion}' es correcta.\n")
            # Dibujar el AST
            dibujar_ast(ast)
            # Construir y evaluar el árbol semántico
            arbol_semantico = construir_arbol_semantico(ast)
            evaluar_arbol_semantico(arbol_semantico)
            # Dibujar el árbol semántico
            dibujar_arbol_semantico(arbol_semantico)
        else:
            sintactico_text.insert(tk.END, f"La proposición '{proposicion}' es incorrecta.\n")

# Analizador léxico
def analisis_lexico(proposicion):
    tokens = []
    palabras = proposicion.split()
    for palabra in palabras:
        palabra_lower = palabra.lower()
        if palabra_lower == 'y':
            tokens.append(Token(TokenType.Y, palabra))
        elif palabra_lower == 'o':
            tokens.append(Token(TokenType.O, palabra))
        elif palabra_lower == 'si':
            tokens.append(Token(TokenType.SI, palabra))
        elif palabra_lower == 'entonces':
            tokens.append(Token(TokenType.ENTONCES, palabra))
        elif palabra_lower == 'no':
            tokens.append(Token(TokenType.NOT, palabra))
        else:
            tokens.append(Token(TokenType.PALABRA, palabra))
    tokens.append(Token(TokenType.EOL, ''))
    return tokens

# Simulación del analizador sintáctico
def analizar_sintactico(tokens):
    PropNode.counter = 0
    PropNode.props_map = {}

    def parse_expression(pos):
        """Parsea una expresión lógica y retorna un nodo del AST."""
        def parse_primary_expression(pos):
            token = tokens[pos]
            if token.tipo == TokenType.NOT:  # Negación
                child_node, next_pos = parse_primary_expression(pos + 1)
                return NegNode(child_node), next_pos
            elif token.tipo == TokenType.PALABRA:  # Proposición simple
                node = PropNode(token)
                # Verificar si hay más palabras para esta proposición
                next_pos = pos + 1
                while next_pos < len(tokens) and tokens[next_pos].tipo == TokenType.PALABRA:
                    node.addToken(tokens[next_pos])
                    next_pos += 1
                return node, next_pos
            else:
                raise ValueError(f"Error de sintaxis en el token: {token.valor}")

        def parse_and_expression(pos):
            left_node, pos = parse_primary_expression(pos)
            while pos < len(tokens) and tokens[pos].tipo == TokenType.Y:
                pos += 1  # Saltar el token 'y'
                right_node, pos = parse_primary_expression(pos)
                left_node = OperatorNode(OperatorType.AND, left_node, right_node)
            return left_node, pos

        def parse_or_expression(pos):
            left_node, pos = parse_and_expression(pos)
            while pos < len(tokens) and tokens[pos].tipo == TokenType.O:
                pos += 1  # Saltar el token 'o'
                right_node, pos = parse_and_expression(pos)
                left_node = OperatorNode(OperatorType.OR, left_node, right_node)
            return left_node, pos

        def parse_implication_expression(pos):
            if tokens[pos].tipo == TokenType.SI:
                pos += 1  # Saltar el token 'si'
                left_node, pos = parse_expression(pos)
                if tokens[pos].tipo == TokenType.ENTONCES:
                    pos += 1  # Saltar el token 'entonces'
                    right_node, pos = parse_expression(pos)
                    return OperatorNode(OperatorType.IMPLICATION, left_node, right_node), pos
            return parse_or_expression(pos)

        return parse_implication_expression(pos)

    # Comenzar desde el primer token
    try:
        ast, final_pos = parse_expression(0)
        if final_pos != len(tokens) - 1:  # Verificar que se consuma todo
            raise ValueError("Tokens extra después de la expresión válida.")
        return ast
    except Exception as e:
        print(f"Error al construir el AST: {e}")
        return None

# Función para evaluar el árbol semántico
def evaluar_arbol_semantico(node):
    if isinstance(node, PropNode):
        proposiciones = {
            'A1': True,
            'A2': False,
            'A3': True
        }
        node.truth_value = proposiciones.get(node.value, None)
    elif isinstance(node, NegNode):
        evaluar_arbol_semantico(node.child)
        node.truth_value = not node.child.truth_value if node.child.truth_value is not None else None
    elif isinstance(node, OperatorNode):
        evaluar_arbol_semantico(node.left)
        evaluar_arbol_semantico(node.right)
        if node.operator == OperatorType.AND:
            node.truth_value = (node.left.truth_value and node.right.truth_value) if (node.left.truth_value is not None and node.right.truth_value is not None) else None
        elif node.operator == OperatorType.OR:
            node.truth_value = (node.left.truth_value or node.right.truth_value) if (node.left.truth_value is not None and node.right.truth_value is not None) else None
        elif node.operator == OperatorType.IMPLICATION:
            node.truth_value = (not node.left.truth_value or node.right.truth_value) if (node.left.truth_value is not None and node.right.truth_value is not None) else None

# Función para dibujar el AST
def dibujar_ast(node):
    """Dibuja el árbol abstracto de sintaxis (AST) usando networkx y matplotlib."""
    G = nx.DiGraph()
    pos = {}  # Almacena las posiciones de los nodos para graficarlos

    def agregar_nodo(nodo, parent_id=None, depth=0, pos_x=0):
        """Función recursiva para agregar nodos y edges al grafo."""
        node_id = id(nodo)  # Identificador único para cada nodo
        label = ""
        if isinstance(nodo, PropNode):
            label = f"Prop: {nodo.id}"
        elif isinstance(nodo, NegNode):
            label = "NOT"
        elif isinstance(nodo, OperatorNode):
            operator_map = {
                OperatorType.OR: "∨",
                OperatorType.AND: "∧",
                OperatorType.IMPLICATION: "⇒"
            }
            label = f"{nodo.id} ({operator_map[nodo.operator]})"

        # Agregar nodo al grafo
        G.add_node(node_id, label=label)
        pos[node_id] = (pos_x, -depth)  # Coordenadas del nodo

        # Si tiene padre, agregar una arista
        if parent_id is not None:
            G.add_edge(parent_id, node_id)

        # Recursión para hijos
        if isinstance(nodo, NegNode):
            agregar_nodo(nodo.child, node_id, depth + 1, pos_x)
        elif isinstance(nodo, OperatorNode):
            # Izquierda y derecha, con desplazamiento en X
            agregar_nodo(nodo.left, node_id, depth + 1, pos_x - 1)
            agregar_nodo(nodo.right, node_id, depth + 1, pos_x + 1)

    # Llamar a la función recursiva con el nodo raíz
    agregar_nodo(node)

    # Dibujar el grafo usando NetworkX
    plt.figure(figsize=(12, 8))
    nx.draw(G, pos, with_labels=True, labels=nx.get_node_attributes(G, 'label'),
            node_size=2000, node_color='lightblue', font_size=10, font_weight='bold', edge_color='gray')
    plt.title("Árbol Abstracto de Sintaxis (AST)", fontsize=14)
    plt.show()

# Función para dibujar el árbol semántico
def dibujar_arbol_semantico(node):
    """Dibuja el árbol semántico usando networkx y matplotlib."""
    G = nx.DiGraph()
    pos = {}  # Almacena las posiciones de los nodos para graficarlos

    def agregar_nodo(nodo, parent_id=None, depth=0, pos_x=0, full_expression=""):
        """Función recursiva para agregar nodos y edges al grafo."""
        node_id = id(nodo)  # Identificador único para cada nodo
        label = full_expression if full_expression else construir_expresion_logica(nodo)

        # Agregar nodo al grafo
        G.add_node(node_id, label=label)
        pos[node_id] = (pos_x, -depth)  # Coordenadas del nodo

        # Si tiene padre, agregar una arista
        if parent_id is not None:
            G.add_edge(parent_id, node_id)

        # Recursión para hijos
        if isinstance(nodo, NegNode):
            agregar_nodo(nodo.child, node_id, depth + 1, pos_x, f"¬({label})")
        elif isinstance(nodo, OperatorNode):
            left_expr = f"{construir_expresion_logica(nodo.left)}"
            right_expr = f"{construir_expresion_logica(nodo.right)}"
            agregar_nodo(nodo.left, node_id, depth + 1, pos_x - 1, left_expr)
            agregar_nodo(nodo.right, node_id, depth + 1, pos_x + 1, right_expr)

    # Llamar a la función recursiva con el nodo raíz
    agregar_nodo(node, full_expression=construir_expresion_logica(node))

    # Dibujar el grafo usando NetworkX
    plt.figure(figsize=(12, 8))
    nx.draw(G, pos, with_labels=True, labels=nx.get_node_attributes(G, 'label'),
            node_size=2000, node_color='lightgreen', font_size=10, font_weight='bold', edge_color='gray')
    plt.title("Árbol Semántico", fontsize=14)
    plt.show()

# Aquí se implementa una lógica diferente para construir el árbol semántico
def construir_arbol_semantico(ast_node):
    """Construye un árbol semántico basado en el AST"""
    if isinstance(ast_node, PropNode):
        return ast_node
    elif isinstance(ast_node, NegNode):
        child_semantic_node = construir_arbol_semantico(ast_node.child)
        return NegNode(child_semantic_node)
    elif isinstance(ast_node, OperatorNode):
        left_semantic_node = construir_arbol_semantico(ast_node.left)
        right_semantic_node = construir_arbol_semantico(ast_node.right)
        return OperatorNode(ast_node.operator, left_semantic_node, right_semantic_node)
    else:
        raise ValueError("Tipo de nodo desconocido")

# Crear una nueva ventana para mostrar la tabla de asignaciones
def mostrar_tabla_asignaciones(ast):
    # Crear una nueva ventana
    ventana_tabla = tk.Toplevel()
    ventana_tabla.title("Asignaciones de Variables")
    ventana_tabla.geometry("800x400")

    # Crear el Treeview
    tree = ttk.Treeview(ventana_tabla, columns=("Proposición Compleja", "Variable Compleja", "Proposición Simple", "Variable Simple"), show='headings')
    tree.heading("Proposición Compleja", text="Proposición Compleja")
    tree.heading("Variable Compleja", text="Variable Compleja")
    tree.heading("Proposición Simple", text="Proposición Simple")
    tree.heading("Variable Simple", text="Variable Simple")
    tree.pack(fill=tk.BOTH, expand=True)

    # Insertar datos en la tabla
    def insertar_datos(nodo, variable_compleja=None):
        if isinstance(nodo, PropNode):
            tree.insert("", "end", values=(None, variable_compleja, nodo.value, nodo.id))
        elif isinstance(nodo, NegNode):
            insertar_datos(nodo.child, variable_compleja)
        elif isinstance(nodo, OperatorNode):
            tree.insert("", "end", values=(construir_expresion_logica(nodo), nodo.id, None, None))
            insertar_datos(nodo.left, nodo.id)
            insertar_datos(nodo.right, nodo.id)

    insertar_datos(ast)
    ventana_tabla.mainloop()

# Función de análisis en la interfaz
def realizar_analisis():
    PropNode.counter = 0
    PropNode.props_map = {}

    proposiciones = entrada_texto.get("1.0", tk.END).strip().split('\n')
    if not proposiciones or not proposiciones[0].strip():
        messagebox.showerror("Error", "Por favor, ingrese una o más proposiciones.")
        return

    lexico_text.delete(1.0, tk.END)
    lexico_text.insert(tk.END, "Tipo\tValor\n")
    sintactico_text.delete(1.0, tk.END)

    for proposicion in proposiciones:
        # Analizador Léxico
        tokens = analisis_lexico(proposicion.strip())
        for token in tokens:
            lexico_text.insert(tk.END, f"{token.tipo}\t{token.valor}\n")

        # Analizador Sintáctico
        ast = analizar_sintactico(tokens)

        if ast:
            sintactico_text.insert(tk.END, f"La proposición '{proposicion}' es correcta.\n")
        else:
            sintactico_text.insert(tk.END, f"La proposición '{proposicion}' es incorrecta.\n")

    # Crear lista de fórmulas proposicionales
    global lista_formulas
    lista_formulas = crear_lista_formulas(proposiciones)

    # Generar botones para cada proposición
    generar_botones_proposiciones(proposiciones)

# Interfaz gráfica
# Interfaz gráfica
ventana = tk.Tk()
ventana.title("Análisis de Proposiciones Lógicas")
ventana.geometry("800x600")

# Entrada de texto
tk.Label(ventana, text="Proposiciones (una por línea):").pack()
entrada_texto = tk.Text(ventana, height=5, width=50)
entrada_texto.pack(pady=5)

# Botón para cargar desde archivo
boton_cargar_archivo = tk.Button(ventana, text="Cargar desde archivo", command=cargar_desde_archivo)
boton_cargar_archivo.pack(pady=5)

# Botón de análisis
boton_analizar = tk.Button(ventana, text="Analizar", command=realizar_analisis)
boton_analizar.pack(pady=5)

# Frame para los botones de proposiciones
frame_botones = tk.Frame(ventana)
frame_botones.pack(pady=5)

# Textos de salida
tk.Label(ventana, text="Análisis Léxico:").pack()
lexico_text = tk.Text(ventana, height=10, width=80)
lexico_text.pack(pady=5)

tk.Label(ventana, text="Análisis Sintáctico:").pack()
sintactico_text = tk.Text(ventana, height=10, width=80)
sintactico_text.pack(pady=5)

# Caja de texto para nueva frase
tk.Label(ventana, text="Nueva Frase:").pack()
nueva_frase_texto = tk.Entry(ventana, width=50)
nueva_frase_texto.pack(pady=5)

# Botón para comparar nueva frase
boton_comparar = tk.Button(ventana, text="Comparar Frase", command=comparar_frase)
boton_comparar.pack(pady=5)

# Iniciar la interfaz
ventana.mainloop()
