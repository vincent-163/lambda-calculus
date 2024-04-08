## Examples

```bash
# Print a term

$ coc two
(a:* (b:(.a a) (a:a (b (b a)))))


# Print the type of an expression

$ coc type "(exp two two)"
Nat


# Fully print the type of an expression

$ coc full type "(exp two two)"
(a.* (.(.a a) (.a a)))


# Print the normal form of an expression

$ coc norm "(exp two two)"
four


# Fully print the normal form of an expression

$ coc full norm "(exp two two)"
(a:* (b:(.a a) (a:a (b (b (b (b a)))))))
```
