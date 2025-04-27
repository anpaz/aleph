
# **Introduction: Aleph: A Language for Sculpting Quantum Probability Distributions**

This language describes quantum computation in terms of the **manipulation of probability distributions** and **coherence structures** over quantum registers.  
It offers a higher-level abstraction than traditional gate-based quantum programming, focusing on **basis-relative transformations** rather than explicit sequences of low-level operations.

A **program** in this language consists of:

- Initializing quantum registers in specific bases,
- Applying **skew** and **twist** operations that modify probability distributions and phase structures,
- **Changing bases** to reinterpret registers before further transformations,
- **Applying operations conditionally**, based on the current logical states of other registers,
- **Measuring** the registers at the end of computation.

Registers maintain an internal record of their basis.  
Transformations are **basis-aware** and automatically adapt to the register’s current orientation, ensuring all operations remain unitary.  
Measurements are always performed in the **Standard** basis, corresponding to the classical Z-basis (|0⟩ and |1⟩).

Conditional operations allow for **coherent control**:  
transformations can be selectively applied to portions of the probability distribution without collapsing the overall quantum state.

This model provides a clean separation between **coherent quantum evolution** and **classical measurement**,  
allowing structured manipulation of probability fields before collapse into classical outcomes.

---

# **Examples**

---

### **Example 1: Simple Probability Skewing**

```plaintext
register A [basis = Standard];

skew A with strength=0.5;
measure A;
```

**Explanation:**

- A register `A` is initialized in the Standard basis.
- A `skew` operation gently rebalances its probability distribution,  
stretching amplitudes toward or away from certain basis states based on the register's current configuration.
- The register is then measured, collapsing it into a classical |0⟩ or |1⟩ outcome according to the final distribution.

---

### **Example 2: Basis Change and Conditional Skew**

```plaintext
register A [basis = Standard];
register B [basis = Standard];

change_basis B to Uniform;

when A in ["00", "11"]:
    skew B with strength=0.7;

measure A;
measure B;
```

**Explanation:**

- Two registers, `A` and `B`, are initialized in the Standard basis.
- The basis of `B` is changed to Uniform (balanced superposition).
- A conditional operation is applied: if `A` is in state |00⟩ or |11⟩,  
then `B` is skewed within the Uniform basis, adjusting its distribution relative to superposed states.
- Finally, both `A` and `B` are measured in the Standard basis.

-----------------------

# aleph

`aleph` defines a high level programming model that can be embedded into classical languages to develop large scale quantum hybrid applications, without the quantum mechanics.

It leverages quantum programming principles like *superposition*, *entanglement*, *quantum parallelims* and *amplitude amplification* in a way that is easy to understand and safe to combine with classical computation to enable powerful hybrid applications.

## Getting started

`aleph` is available as a package for both, [F# - aleph.lang](https://www.nuget.org/packages/aleph.lang) and [Python - aleph-lang](https://pypi.org/project/aleph-lang/). Take a look at the documentation of the corresponding packages to get started in that language.

## Building from source

Instructions on how to get your environment setup to build and run `aleph` locally can be found in [src/README.md](src/README.md).

