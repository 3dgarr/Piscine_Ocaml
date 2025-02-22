# 📜 Day-specific rules

## **1️⃣ Tail Recursion**

✅ **By default, your functions do NOT need to be tail-recursive.**
✅ If an exercise **explicitly** states that a function must be **tail-recursive**, then performance is a concern.
✅ In such cases, your function **must run in O(n) or better**.
❌ Any function that runs **slower than O(n)** (e.g., exponential time) will receive **zero points**.

💡 **Why?** Tail-recursive functions avoid excessive stack usage and improve performance.

---

## **2️⃣ Performance Considerations**

✅ Some exercises require **intensive calculations**, so it’s **okay** if they run slowly.
✅ If your function causes a **stack overflow**, you **will NOT lose points**.
❌ However, **infinite recursion** results in **zero points** for the exercise.

💡 **Tip:** Ensure your function always reaches a base case to avoid infinite recursion.

---

## **3️⃣ No Loops Allowed**

❌ **Absolutely no**`while` or `for` loops—only recursion is allowed! 🚫
❌ Using loops is considered **cheating**.

💡 **Alternative:** Use recursive functions instead of iteration.

---

## **4️⃣ Code Structure**

✅ Your solution **must be contained within a single `let` definition** (unless stated otherwise).
✅ You **can** use **nested function definitions** inside your main function.
❌ **Multiple top-level `let` definitions are NOT allowed.**

### ✅ **Correct Example (Nested Function Allowed)**

```ocaml
let my_function x =
  let rec helper y =
    (* recursive logic *)
  in
  helper x
```

### ❌ **Wrong Example (Multiple `let` Definitions)**

```ocaml
let helper y = (* ❌ Wrong: Separate top-level definition *)
  (* recursive logic *)

let my_function x = (* ❌ Wrong: Another separate top-level function *)
  helper x
```

---

## **5️⃣ Complete Working Programs**

✅ Each exercise submission **must be a complete OCaml program**.
✅ This means your file must contain:

1. **One `let` definition** for the function.
2. **One `let ()` block** to run and demonstrate examples.

### ✅ **Correct Format**

```ocaml
let factorial n =
  let rec aux acc x =
    if x = 0 then acc
    else aux (acc * x) (x - 1)
  in
  aux 1 n

let () =
  print_int (factorial 5); print_newline (); (* Expected output: 120 *)
  print_int (factorial 0); print_newline (); (* Expected output: 1 *)
```

💡 **Important:** The provided examples in the problem statement are usually **not enough**—you must add more to prove correctness.

---

## **6️⃣ Standard Library Restrictions**

❌ You **cannot** use **any OCaml standard library functions** in your solutions.
✅ However, you **can** use them inside the `let ()` block for testing.

### ✅ **Allowed (Using Standard Functions in `let ()`)**

```ocaml
let () =
  print_string "Hello, world!\n"  (* Allowed in testing *)
```

### ❌ **Cheating (Using Standard Library Functions in Solution)**

```ocaml
let double_list lst =
  List.map (fun x -> x * 2) lst  (* ❌ Cheating: Not allowed! *)
```

💡 **Tip:** You must implement list processing and other utilities manually.

---

## **7️⃣ Limited OCaml Features**

❌ **No `match` expressions**—they are strictly forbidden!
✅ Use `if` expressions instead.

### ❌ **Wrong (Using `match`)**

```ocaml
let is_even n =
  match n mod 2 with
  | 0 -> true
  | _ -> false  (* ❌ Cheating! *)
```

### ✅ **Correct (Using `if`)**

```ocaml
let is_even n =
  if n mod 2 = 0 then true else false  (* ✅ Allowed *)
```

---

## **8️⃣ Operators Are Allowed**

✅ **All OCaml operators** (`+`, `-`, `*`, `/`, `mod`, etc.) are **allowed**.
✅ Operators are functions from the `Pervasives` module, so they are permitted.

---

# 🏁 **Final Summary**

✅ **Tail recursion is required only when specified.**
✅ **Your function must run in O(n) or better if performance is required.**
✅ **Stack overflow is okay, but infinite recursion = zero points.**
❌ **No `while` or `for` loops—recursion only.**
❌ **Only one `let` definition per exercise, but nested functions are okay.**
✅ **Every file must be a complete working program with a `let ()` block.**
❌ **No standard library functions inside solutions (but okay in `let ()`).**
❌ **No `match ... with` expressions—use `if` instead.**
✅ **Operators (`+`, `-`, `*`, `/`, `mod`, etc.) are allowed.**

#
